// json2dir: create the directory tree a JSON document describes, in the current directory.
// Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
// Scala 3 stdlib + JDK only: the JSON parser is hand-written.

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, LinkOption, NoSuchFileException, Path, StandardOpenOption}
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermission}
import scala.collection.mutable

class Fail(message: String) extends RuntimeException(message)

// The parsed document. Other holds numbers/booleans/null, kept only to be reported by validate().
enum Node:
  case Obj(members: mutable.LinkedHashMap[String, Node])
  case Arr(items: List[Node])
  case Str(value: String)
  case Other(what: String)

// ---- §3: strict UTF-8 and strict JSON ----

def decode(bytes: Array[Byte]): String =
  try
    StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
      .decode(ByteBuffer.wrap(bytes)).toString
  catch case _: CharacterCodingException => throw Fail("input is not valid UTF-8")

class Parser(s: String):
  // A leading byte order mark is ignored, as §3 allows.
  private var i = if s.startsWith("﻿") then 1 else 0

  private def error(what: String) = Fail(s"input is not valid JSON: $what at offset $i")
  private def peek(c: Char) = i < s.length && s(i) == c

  private def ws(): Unit =
    while i < s.length && " \t\n\r".indexOf(s(i)) >= 0 do i += 1

  def document(): Node =
    ws()
    val v = value()
    ws()
    if i != s.length then throw error("unexpected data after the value")
    v

  private def value(): Node =
    if i >= s.length then throw error("unexpected end of input")
    s(i) match
      case '{' => obj()
      case '[' => arr()
      case '"' => Node.Str(string())
      case 't' => literal("true")
      case 'f' => literal("false")
      case 'n' => literal("null")
      case c if c == '-' || (c >= '0' && c <= '9') => number()
      case _ => throw error("unexpected character")

  private def literal(word: String): Node =
    if !s.startsWith(word, i) then throw error("invalid literal")
    i += word.length
    Node.Other(word)

  private def digits(): Boolean =
    val start = i
    while i < s.length && s(i) >= '0' && s(i) <= '9' do i += 1
    i > start

  private def number(): Node =
    if peek('-') then i += 1
    if peek('0') then i += 1 else if !digits() then throw error("invalid number")
    if peek('.') then
      i += 1
      if !digits() then throw error("invalid number")
    if peek('e') || peek('E') then
      i += 1
      if peek('+') || peek('-') then i += 1
      if !digits() then throw error("invalid number")
    Node.Other("number")

  // After a member or item: returns true at the closing bracket, false at a comma.
  private def separator(close: Char): Boolean =
    ws()
    if i >= s.length then throw error("unexpected end of input")
    val c = s(i)
    if c == close then { i += 1; true }
    else if c == ',' then { i += 1; false }
    else throw error(s"expected ',' or '$close'")

  private def obj(): Node =
    i += 1 // {
    val members = mutable.LinkedHashMap.empty[String, Node]
    ws()
    if peek('}') then { i += 1; return Node.Obj(members) }
    var done = false
    while !done do
      ws()
      if !peek('"') then throw error("expected a member name")
      val name = string()
      ws()
      if !peek(':') then throw error("expected ':'")
      i += 1
      ws()
      val v = value()
      members.remove(name) // duplicates: the last occurrence wins
      members(name) = v
      done = separator('}')
    Node.Obj(members)

  private def arr(): Node =
    i += 1 // [
    val items = mutable.ListBuffer.empty[Node]
    ws()
    if peek(']') then { i += 1; return Node.Arr(Nil) }
    var done = false
    while !done do
      ws()
      items += value()
      done = separator(']')
    Node.Arr(items.toList)

  private def hex4(): Int =
    if i + 4 > s.length then throw error("bad \\u escape")
    val h = s.substring(i, i + 4)
    if !h.forall(c => Character.digit(c, 16) >= 0) then throw error("bad \\u escape")
    i += 4
    Integer.parseInt(h, 16)

  private def string(): String =
    i += 1 // opening quote
    val sb = StringBuilder()
    while true do
      if i >= s.length then throw error("unterminated string")
      val c = s(i)
      i += 1
      if c == '"' then return sb.toString
      else if c < ' ' then { i -= 1; throw error("control character in string") }
      else if c != '\\' then sb += c
      else
        if i >= s.length then throw error("unterminated string")
        val e = s(i)
        i += 1
        e match
          case '"' => sb += '"'
          case '\\' => sb += '\\'
          case '/' => sb += '/'
          case 'b' => sb += '\b'
          case 'f' => sb += '\f'
          case 'n' => sb += '\n'
          case 'r' => sb += '\r'
          case 't' => sb += '\t'
          case 'u' =>
            val u = hex4()
            if u >= 0xDC00 && u <= 0xDFFF then throw error("unpaired surrogate")
            if u >= 0xD800 && u <= 0xDBFF then
              // §3.5: a high surrogate must be followed by an escaped low surrogate.
              if !s.startsWith("\\u", i) then throw error("unpaired surrogate")
              i += 2
              val lo = hex4()
              if lo < 0xDC00 || lo > 0xDFFF then throw error("unpaired surrogate")
              sb += u.toChar
              sb += lo.toChar
            else sb += u.toChar
          case _ => i -= 1; throw error("bad escape")
    throw error("unreachable")

// ---- §4, §6: validate the whole document before touching the file system ----

def quote(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

def validate(value: Node, where: String): Unit = value match
  case Node.Str(_) => ()
  case Node.Other(what) => throw Fail(s"$where: $what values are not allowed")
  case Node.Arr(List(Node.Str(kind), Node.Str(payload))) =>
    if kind != "link" && kind != "script" then throw Fail(s"$where: unknown array kind ${quote(kind)}")
    // §10: a link target with NUL cannot be created, so it is rejected before anything is written.
    if kind == "link" && payload.contains('\u0000') then throw Fail(s"$where: a link target cannot contain NUL")
  case Node.Arr(_) =>
    throw Fail(s"""$where: an array must be ["link", target] or ["script", content]""")
  case Node.Obj(members) =>
    for (name, child) <- members do
      val path = if where == "." then name else s"$where/$name"
      // §4.2.1: names are used exactly; trailing "/" or "/." forms are rejected, not trimmed.
      if name.isEmpty || name == "." || name == ".." || name.contains('/') || name.contains('\u0000') then
        throw Fail(s"$path: invalid name ${quote(name)}")
      validate(child, path)

// ---- §5: create the tree ----

def utf8(s: String) = s.getBytes(StandardCharsets.UTF_8)

def lstat(p: Path): Option[BasicFileAttributes] =
  try Some(Files.readAttributes(p, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS))
  catch case _: NoSuchFileException => None

// §5.2, §5.3: an existing non-directory is removed (a symlink itself, never its target);
// §5.4: a directory in the way of a non-object is an error.
def clear(p: Path, existing: Option[BasicFileAttributes]): Unit = existing match
  case Some(a) if a.isDirectory => throw Fail(s"$p: a directory is in the way")
  case Some(_) => Files.delete(p)
  case None => ()

def writeFile(p: Path, content: String, executable: Boolean): Unit =
  // CREATE_NEW = O_CREAT | O_EXCL, mode 0666 & ~umask.
  val ch = Files.newByteChannel(p, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
  try
    val buf = ByteBuffer.wrap(utf8(content))
    while buf.hasRemaining do ch.write(buf)
  finally ch.close()
  if executable then
    val perms = Files.getPosixFilePermissions(p, LinkOption.NOFOLLOW_LINKS)
    perms.add(PosixFilePermission.OWNER_EXECUTE)
    perms.add(PosixFilePermission.GROUP_EXECUTE)
    perms.add(PosixFilePermission.OTHERS_EXECUTE)
    Files.setPosixFilePermissions(p, perms)

def apply(dir: Path, members: mutable.LinkedHashMap[String, Node]): Unit =
  // Members in ascending order of the UTF-8 bytes of their names, like the reference.
  val names = members.keys.toArray.sortWith((a, b) => java.util.Arrays.compareUnsigned(utf8(a), utf8(b)) < 0)
  for name <- names do
    val p = dir.resolve(name)
    val existing = lstat(p)
    members(name) match
      case Node.Str(content) =>
        clear(p, existing)
        writeFile(p, content, false)
      case Node.Arr(List(Node.Str(kind), Node.Str(payload))) =>
        clear(p, existing)
        // Known gap: java.nio normalizes the target path (a//b/ becomes a/b).
        if kind == "link" then Files.createSymbolicLink(p, Path.of(payload))
        else writeFile(p, payload, true)
      case Node.Obj(children) =>
        if !existing.exists(_.isDirectory) then
          if existing.isDefined then Files.delete(p)
          Files.createDirectory(p)
        apply(p, children)
      case _ => throw Fail(s"$p: unexpected value") // ruled out by validate()

@main def json2dir(args: String*): Unit =
  if args.nonEmpty then
    System.err.println("usage: json2dir < document.json")
    sys.exit(2)
  try
    Parser(decode(System.in.readAllBytes())).document() match
      case root @ Node.Obj(members) =>
        validate(root, ".")
        apply(Path.of(""), members)
      case _ => throw Fail("the root of the document must be an object")
    sys.exit(0)
  catch
    case e: Fail => System.err.println(s"json2dir: ${e.getMessage}")
    case _: StackOverflowError => System.err.println("json2dir: the document is nested too deeply")
    case e: Exception => System.err.println(s"json2dir: $e")
  sys.exit(1)

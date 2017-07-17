structure MA =
struct
  fun stringreader s =
    let
      val pos = ref 0
      val remainder = ref (String.size s)
      fun min(a, b) = if a < b then a else b
    in
      fn n =>
        let
          val m = min(n, !remainder)
          val s = String.substring(s, !pos, m)
          val () = pos := !pos + m
          val () = remainder := !remainder - m
        in
          s
        end
    end


  exception ParseError of Pos.t * string

  val printLn = print o (fn s => s ^ "\n")

  fun error fileName (s, pos, pos') : unit =
    raise ParseError (Pos.pos (pos fileName) (pos' fileName), s)

  open Coord
  open Pos

  fun main (name, args) =
    let
      val input = (TextIO.inputLine TextIO.stdIn)
    in
      case input of
        NONE => 1
      | SOME str =>
          ((let
              val lexer =
                MAParser.makeLexer (stringreader (Option.valOf input)) "stdin"
              val result = MAParser.parse (1, lexer, error "-", "-")
            in
              printLn "Successfully parsed"
            end
            handle ParseError (p, s) =>
              printLn ("Error: " ^ Pos.toString p)); 0)
    end

end
structure MA =
struct
  structure SD  = SortData
  structure ATA = AstToAbt
  structure SA  = ShowAbt

  fun stringreader s =
    let
      val pos = ref 0
      val remainder = ref (String.size s)
      fun min(a, b) = if a < b then a else b
    in
      fn n =>
        let
          val m  = min (n, !remainder)
          val s  = String.substring (s, !pos, m)
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
      val input = TextIO.input TextIO.stdIn
    in
      let
        val lexer = MAParser.makeLexer (stringreader input) "-"
        val (result, _) = MAParser.parse (1, lexer, error "-", "-")
        val mctx  = Abt.Metavariable.Ctx.empty
        val out = SA.toString (ATA.convert mctx (result, SD.EXP))
      in
        (printLn out; 0)
      end
      handle
        ParseError (p, s) => (printLn ("Error: " ^ Pos.toString p); 1)
      | _ => (printLn "Unknown error."; 1)
    end

  val _ = SMLofNJ.exportFn ("ma", main)
end

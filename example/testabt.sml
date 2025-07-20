
structure TestAbt = struct 
  
  val abt =
    (Abt.parseInstream (TextIO.openIn "example.abt"))
    handle
    Abt.ParseError s => (print s ; raise Abt.ParseError s)

  val () = Abt.print TextIO.stdOut abt;
end

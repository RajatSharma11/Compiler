structure Driver =
struct 
  
  fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end
  
  fun drive filename =
      let val ast = Parser.parse filename
	      val code = IntermediateCode.compile ast
	      val _ = writeFile "output.txt" (String.concat(code))
       in 
          ()
      end

end


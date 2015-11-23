This is a direct transliteration into F# of Graham Hutton's Haskell parser 
combinators library from chapter 8 of his book, *Programming in Haskell*.

It's not always quite idiomatic F# style, but I tried to stay pretty close to
the code in the book.

To run: Open solution, restore NuGet packages, run NUnit tests.

##### Known issues
F# compiler warning: 
> This and other recursive references to the object(s) being defined will be 
> checked for initialization-soundness at runtime through the use of a delayed 
> reference. This is because you are defining one or more recursive objects, 
> rather than recursive functions. This warning may be suppressed by using 
> '#nowarn "40"' or '--nowarn:40'.	

> ParserCombinators	Hutton\ParserCombinators\Parser.fs	151

I think F# may be off here as the functions in question are, well, functions, 
but I'm still looking
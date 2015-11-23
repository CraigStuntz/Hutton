namespace TinyLangage.Tests.Lexer

open NUnit.Framework
open FsUnit
open Parsers

type ParserTests () =
    [<Test>]
    member this.``return 1 works``() =
        let actual = parse (parser { return 1 }) "abc"
        actual |> should equal [(1, "abc")]

    [<Test>]
    member this.``failure returns empty list``() =
        let actual = parse failure "abc"
        actual |> should equal []

    [<Test>]
    member this.``item with empty string returns empty list``() =
        let actual = parse item ""
        actual |> should equal []
    
    [<Test>]
    member this.``item "abc" returns 'a'``() =
        let actual = parse item "abc"
        actual |> should equal [('a', "bc")]

    [<Test>]
    member this.``computation expressions with Bind and Return work``() =
        let p : Parser<char * char> = 
            parser {
                let! x = item
                let! _ = item
                let! y = item
                return (x, y)
            }
        let actual = parse p "abcdef" 
        actual |> should equal [(('a', 'c'), "def")]

    [<Test>]
    member this.``computation expressions with Bind and Return work on bad match``() =
        let p : Parser<char * char> = 
            parser {
                let! x = item
                let! _ = item
                let! y = item
                return (x, y)
            }
        let actual = parse p "ab" 
        actual |> should equal []

    [<Test>]
    member this.``choice with p``() = 
        let actual = parse (item +++ (parser { return 'd'})) "abc"
        actual |> should equal [('a', "bc")]

    [<Test>]
    member this.``choice with q``() = 
        let actual = parse (failure +++ (parser { return 'd'})) "abc"
        actual |> should equal [('d', "abc")]

    [<Test>]
    member this.``choice with failure``() = 
        let actual = parse (failure +++ failure) "abc"
        actual |> should equal []

    [<Test>]
    member this.``digit parses "1"``() =
        let actual = parse digit "123"
        actual |> should equal [('1', "23")]
    
    [<Test>]
    member this.``digit fails on "abc"``() =    
        let actual = parse digit "abc"
        actual |> should equal []
        
    [<Test>]
    member this.``char parses "a"``() =
        let actual = parse (charParser 'a') "abc"
        actual |> should equal [('a', "bc")]
    
    [<Test>]
    member this.``char fails on "123"``() =
        let actual = parse (charParser 'a') "123"
        actual |> should equal []
    
    [<Test>]
    member this.``stringParser parses "abc"``() =
        let actual = parse (stringParser "abc") "abcdef"
        actual |> should equal [("abc", "def")]
    
    [<Test>]
    member this.``many digit parses "123abc"``() =
        let actual = parse (many digit) "123abc"
        actual |> should equal [(['1'; '2'; '3'], "abc")]

    [<Test>]
    member this.``many digit parses "abcdef"``() =
        let actual = parse (many digit) "abcdef"
        // F# type inference gets the type of [] wrong and "should equal" fails 
        // if we don't give it explicit help!
        // This is mostly a bug in NUnit/FsUnit: https://github.com/fsprojects/FsUnit/issues/78
        let expectedList : char list = []
        let expected = [(expectedList, "abcdef")]
        actual |> should equal expected

    [<Test>]
    member this.``many1 digit fails on "abcdef"``() =
        let actual = parse (many1 digit) "abcdef"
        actual |> should equal []

    [<Test>]
    member this.``ident works``() =
        let actual = parse ident "abc def"
        actual |> should equal [("abc", " def")]

    [<Test>]
    member this.``nat works``() =
        let actual = parse nat "123 abc"
        actual |> should equal [(123, " abc")]

    [<Test>]
    member this.``space works``() =
        let actual = parse space "    abc"
        actual |> should equal [((), "abc")]

    [<Test>]
    member this.``can parse non-empty list of natural numbers and ignore spacing``() = 
        let p : Parser<int list> = 
            let natAndComma = parser {
                let! _ = symbol ","
                return! natural
            }
            parser {
                let! _ = symbol "["
                let! n = natural
                let! ns = many natAndComma
                let! _ = symbol "]"
                return (n :: ns)
            }
        parse p " [1, 2, 3] " |> should equal [([1; 2; 3], "")]
        parse p "[1, 2, ]"    |> should equal []


    [<Test>]
    member this.``can eval 2*3+4``() = 
        let actual = eval "2*3+4"
        actual |> should equal 10

    [<Test>]
    member this.``can eval 2*(3+4)``() = 
        let actual = eval "2*(3+4)"
        actual |> should equal 14

    [<Test>]
    member this.``eval fails on 2*3-4``() = 
        (fun () -> eval "2*3-4" |> ignore) |> should (throwWithMessage "Unused input -4") typeof<System.Exception>

    [<Test>]
    member this.``eval fails on -1``() = 
        (fun () -> eval "-1" |> ignore) |> should (throwWithMessage "Invalid input -1") typeof<System.Exception>
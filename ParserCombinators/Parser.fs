module Parsers

open System


type Parser<'a>      = (char list) -> ('a * (char list)) list


type ParserBuilder() =

    member this.Bind(p, f)    = p >> List.collect (fun (a, inp) -> (f a) inp)

    member this.Return(x)     = fun (inp: (char list)) -> [(x, inp)]

    member this.ReturnFrom(m) = m

    member this.Zero()        = []


let parser = new ParserBuilder()


let failure : Parser<'a> =
    fun _ -> []


/// Fails if input char list is empty.
/// Succeeds with first char otherwise
let item : Parser<char> = function
| []      -> []
| x :: xs -> [(x, xs)]


let parse (p : Parser<'a>) (inp : string) : ('a * string) list = 
    p (inp |> List.ofSeq) 
        |> List.map (fun (a, chars) -> (a, chars |> String.Concat))


let (+++) (p: Parser<'a>) (q: Parser<'a>) : Parser<'a> = 
    fun (inp: char list) ->
        match inp |> p with
        | []      -> inp |> q
        | success -> success


let sat (p: char -> bool) : Parser<char> =
    parser {
        let! x = item
        if (p x)
        then return x
        else return! failure
    }


let digit: Parser<char> = 
    sat Char.IsDigit


let lower: Parser<char> =
    sat Char.IsLower


let upper: Parser<char> =
    sat Char.IsUpper


let letter: Parser<char> = 
    sat Char.IsLetter


let alphanum: Parser<char> =
    sat Char.IsLetterOrDigit


let charParser (x: char) : Parser<char> =
    sat ((=) x)


let rec stringParser (str: string) : Parser<string> =
    parser {
        match str with
        | "" -> return ""
        | _  -> 
            let! _ = charParser str.[0]
            let! _ = stringParser (str.Substring 1)
            return str
    }


let rec many (p: Parser<'a>) : Parser<'a list> =
    many1 p +++ parser { return [] }
and many1 (p: Parser<'a>) : Parser<'a list> =
    parser {
        let! v = p
        let! vs = many p
        return (v :: vs)
    }


let ident : Parser<string> =
    parser {
        let!   x  = lower
        let!   xs = many alphanum
        return 
            x :: xs 
            |> String.Concat
    }


let nat : Parser<int> =
    parser {
        let!   xs = many1 digit
        return 
            xs 
            |> String.Concat 
            |> Int32.Parse
    }


let space : Parser<unit> =
    parser {
        let! _ = many (sat Char.IsWhiteSpace)
        return ()
    }


let token (p: Parser<'a>) : Parser<'a> =
    parser {
        let! _ = space
        let! v = p
        let! _ = space
        return v
    }


let identifier : Parser<string> =
    token ident


let natural : Parser<int> = 
    token nat


let symbol (xs: string) : Parser<string> =
    token <| stringParser xs




let rec expr : Parser<int> =
    plusExpr +++ term
and plusExpr : Parser<int> =
    parser {
        let! t = term
        let! _ = symbol "+"
        let! e = expr
        return (t + e) 
    }
and term : Parser<int> = 
    timesExpr +++ factor
and timesExpr : Parser<int> =
        parser {
            let! f = factor
            let! _ = symbol "*"
            let! t = term
            return (f * t)
        }
and factor : Parser<int> = 
    parenthetical +++ natural
and parenthetical : Parser<int> = 
    parser {
        let! _ = symbol "("
        let! e = expr
        let! _ = symbol ")"
        return e
    }




let eval (xs: string) : int =
    match parse expr xs with
    | [(n, "")]  -> n
    | [(_, out)] -> failwithf "Unused input %s" out
    | _          -> failwithf "Invalid input %s" xs

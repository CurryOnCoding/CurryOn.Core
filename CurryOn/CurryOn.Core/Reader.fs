namespace CurryOn.Core

type Reader<'environment, 'a> = Reader of ('environment -> 'a)

module Reader = 

    /// Evaluate the action with a given environment
    /// 'env -> Reader<'env,'a> -> 'a
    let run environment (Reader action) = 
        let resultOfAction = action environment
        resultOfAction

    /// ('a -> 'b) -> Reader<'env,'a> -> Reader<'env,'b>
    let map f action = 
        let newAction environment =
            let x = run environment action 
            f x
        Reader newAction

    /// 'a -> Reader<'env,'a>
    let retn x = 
        let newAction environment =
            x
        Reader newAction

    /// Reader<'env,('a -> 'b)> -> Reader<'env,'a> -> Reader<'env,'b>
    let apply fAction xAction = 
        let newAction environment =
            let f = run environment fAction 
            let x = run environment xAction 
            f x
        Reader newAction

    /// ('a -> Reader<'env,'b>) -> Reader<'env,'a> -> Reader<'env,'b>
    let bind f xAction = 
        let newAction environment =
            let x = run environment xAction 
            run environment (f x)
        Reader newAction

    type ReaderBuilder () =
        member __.Bind (x, f) = bind f x
        member __.Return (x) = retn x
        member __.ReturnFrom (x) = x
        member __.Yield (x) = retn x
        member __.YieldFrom (x) = x


[<AutoOpen>]
module ReaderMonad =
    let reader = Reader.ReaderBuilder()
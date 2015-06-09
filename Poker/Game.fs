module Poker.Game

open Poker.Types

let rank = fst
let suit = snd

let (|Sorted|) xs = xs |> Seq.sort |> List.ofSeq

let (|RankGroups|) hand =
    hand |> (Seq.groupBy id >> Seq.map (snd >> List.ofSeq) >> Seq.sortBy (Seq.length >> (~-)) >> List.ofSeq)

let forall p list =
    list
    |> Seq.pairwise
    |> Seq.forall p
    |> (fun value -> if value then Some() else None)
    |> Option.map (fun () -> list |> Seq.head)

let (|Tag|) (a: 't) =
    Microsoft.FSharp.Reflection.FSharpValue.PreComputeUnionTagReader(typeof<'t>) (box a)

let (|StraightInHand|_|) =
    forall (fun (Tag a, Tag b) -> b - a = 1) >> Option.map Seq.head

let (|OfSameSuit|_|) =
    forall (fun (a, b) -> a = b)

let (|NumberOfAKind|_|) n group =
    if group |> Seq.length = n then
        group |> Seq.head |> Some
    else None

let (|FiveKickers|_|) groups =
    match groups with
    | [[kicker1]; [kicker2]; [kicker3]; [kicker4]; [kicker5]] -> (kicker1, kicker2, kicker3, kicker4, kicker5) |> Some
    | _ -> None

let (|FourKickers|_|) groups =
    match groups with
    | [[kicker1]; [kicker2]; [kicker3]; [kicker4]] -> (kicker1, kicker2, kicker3, kicker4) |> Some
    | _ -> None

let (|ThreeKickers|_|) groups =
    match groups with
    | [[kicker1]; [kicker2]; [kicker3]] -> (kicker1, kicker2, kicker3) |> Some
    | _ -> None

let (|TwoKickers|_|) groups =
    match groups with
    | [[kicker1]; [kicker2]] -> (kicker1, kicker2) |> Some
    | _ -> None

let (|OneKicker|_|) groups =
    match groups with
    | [[kicker1]] -> kicker1 |> Some
    | _ -> None

let detect (Sorted hand) =
    let RankGroups rankGroups, suits = hand |> List.unzip
    match rankGroups, suits with
    | StraightInHand rank                                               , OfSameSuit suit   -> StraightFlush (rank)
    | NumberOfAKind 4 rank :: OneKicker kicker                          , _                 -> FourOfAKind (rank, kicker)
    | NumberOfAKind 3 rank :: NumberOfAKind 2 over :: []                , _                 -> FullHouse (rank, over)
    | FiveKickers kickers                                               , OfSameSuit suit   -> Flush (kickers)
    | StraightInHand rank                                               , _                 -> Straight rank
    | NumberOfAKind 3 rank :: TwoKickers kickers                        , _                 -> ThreeOfAKind (rank, kickers)
    | NumberOfAKind 2 rank :: NumberOfAKind 2 over :: OneKicker kicker  , _                 -> TwoPair (rank, over, kicker)
    | NumberOfAKind 2 rank :: ThreeKickers kickers                      , _                 -> OnePair (rank, kickers)
    | NumberOfAKind 1 rank :: FourKickers kickers                       , _                 -> High (rank, kickers)
    | _                                                                 , _                 -> failwithf "unknown hand %A" hand

let handClassifier : Classifier =
    fun hand ->
        hand
        |> Seq.map (fun c -> c.Rank, c.Suit)
        |> detect

let game : Game =
    fun players ->
        let _, topHandRank =
            players
            |> Seq.groupBy (fun p -> p.Hand |> handClassifier)
            |> Seq.sortBy fst
            |> Seq.head

        match topHandRank |> Seq.toList with
        | [winner] -> Winner winner
        | players -> SplitPot players

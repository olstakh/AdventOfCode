let inputFile() = System.IO.File.ReadAllLines("C:\Users\olstakh\Source\Repos\AdventOfCode\AdventOfCode\Input.txt")

// ========================== DAY 1 ========================== //
let calcfuel amount = amount / 3 - 2

let fuel startAmount =
    Seq.unfold(fun amount -> Some(amount, calcfuel amount)) startAmount
    |> Seq.takeWhile(fun amount -> amount > 0)
    |> Seq.sum
    |> fun total -> total - startAmount

// ========== SOLUTION 1 ========== //
inputFile()
|> Array.map int
|> Array.sumBy calcfuel
|> printfn "%d"

// ========== SOLUTION 2 ========== //
inputFile()
|> Array.map int
|> Array.sumBy fuel
|> printfn "%d"

// ========================== DAY 2 ========================== //
let Substitute idx value array =
    Array.set array idx value
    array

let givenProgram() = (inputFile() |> Array.head).Split(',') |> Array.map int

// ========== SOLUTION 1 ========== //
givenProgram()
|> Array.copy
|> Intcode.RunProgram 0 |> fst
|> Array.head
|> printfn "%d"

// ========== SOLUTION 2 ========== //
let expectedAnswer = 19690720
let inc = function | (a, 99) -> (a+1, 0) | (a, b) -> (a, b+1)

Seq.unfold(fun current -> Some(current, inc current)) (0,0)
|> Seq.takeWhile(fun (noun, _) -> noun < 100)
|> Seq.find(fun (noun, verb) -> givenProgram() |> Array.copy |> Substitute 1 noun |> Substitute 2 verb |> Intcode.RunProgram 0 |> fst |> Array.head = expectedAnswer)
|> fun (noun, verb) -> 100 * noun + verb
|> printfn "%d"

// ========================== DAY 3 ========================== //

let addTuple (a, b) (c, d) = (a+c, b+d)
let manhattan (a, b) (c, d) = abs (a-c) + abs (b-d)

let TracePath currentPath (newDirection:string) =
    let relativeDirection = function
        | 'U' -> (+1, 0)
        | 'D' -> (-1, 0)
        | 'L' -> (0, -1)
        | 'R' -> (0, +1)
        | bad -> failwithf "bad input. %c is not a direction" bad
    let startFrom = currentPath |> List.last
    let direction = newDirection.[0] |> relativeDirection
    let move position _ = addTuple position direction

    let length = newDirection.Substring(1) |> int
    let newPath = [1 .. length] |> List.scan move startFrom |> List.tail

    currentPath @ newPath

let GetWirePath wireIdx = 
    wireIdx
    |> Array.get (inputFile())
    |> fun path -> path.Split(',') |> Array.toList
    |> List.fold TracePath [(0,0)]
    |> List.tail

let wire1Path = GetWirePath 0
let wire2Path = GetWirePath 1

let StepsTaken intersection =
    let idx1 = wire1Path |> List.findIndex ((=)intersection)
    let idx2 = wire2Path |> List.findIndex ((=)intersection)
    idx1 + idx2 + 2

let BestIntersectionBy criteria =
    Set.intersect (wire1Path |> Set.ofList) (wire2Path |> Set.ofList)
    |> Set.toList
    |> List.map criteria
    |> List.min


// ========== SOLUTION 1 ========== //
BestIntersectionBy (manhattan (0,0)) |> printfn "%d"

// ========== SOLUTION 2 ========== //
BestIntersectionBy StepsTaken |> printfn "%d"

// ========================== DAY 4 ========================== //

let hasDouble n = n.ToString() |> Seq.pairwise |> Seq.exists(fun (d1, d2) -> d1 = d2)
let digitsNonDesc n = n.ToString() |> Seq.pairwise |> Seq.forall(fun (d1, d2) -> d1 <= d2)
let uniqueDouble n = n.ToString() |> Seq.pairwise |> Seq.filter(fun (d1, d2) -> d1 = d2) |> Seq.groupBy id |> Seq.exists(fun (d, dSeq) -> Seq.length dSeq = 1)

// ========== SOLUTION 1 ========== //
[256310 .. 732736] |> List.filter hasDouble |> List.filter digitsNonDesc |> List.length |> printfn "%d"

// ========== SOLUTION 2 ========== //
[256310 .. 732736] |> List.filter hasDouble |> List.filter digitsNonDesc |> List.filter uniqueDouble |> List.length |> printfn "%d"

// ========================== DAY 5 ========================== //


let w = 25
let h = 6
let img = inputFile() |> Array.head

let cnt  ch arr = arr |> Array.filter((=)(ch)) |> Array.length
let layers = img |> Seq.splitInto(img.Length / w / h)

let tt =
    [0 .. w * h - 1]
    |> List.map(fun t -> layers |> Seq.pick(fun layer -> let ch = Array.get layer t in if ch = '2' then None else Some(ch)))
    |> List.toSeq
    |> Seq.splitInto h
    |> Seq.toList
    |> List.map(System.String)


let allAsteroids = inputFile() |> Array.mapi(fun row line -> (row, line)) |> Array.collect(fun (row, line) -> line |> Seq.mapi(fun col ch -> (ch, (row, col))) |> Seq.filter(fst >> (=)('#')) |> Seq.map snd |> Seq.toArray)

let rec gcd a b = if b = 0 then a else gcd b (a%b)
let rec gcdL a b = if b = 0L then a else gcdL b (a%b)

let normalize (a, b) =
    let g = gcd (abs a) (abs b)
    (a/g, b/g)
let dist2 (a,b) (c,d) = (a-c)*(a-c) + (b-d)*(b-d)

let CountVisible arr (r, c) =
    let getAngle (r1, c1) = normalize (r-r1, c-c1)

    arr |> Array.filter((<>)(r, c)) |> Array.toSeq |> Seq.groupBy getAngle |> Seq.length

let cross (x1,y1) (x2,y2) = x1*y2-x2*y1

let sign = function
    | neg when neg < 0 -> -1
    | pos when pos > 0 -> +1
    | _ -> 0

let areSame (r3,c3) (r1, c1) (r2, c2)  =
    //normalize (r1-r3,c1-c3) = normalize(r2-r3,c2-c3)

    cross (r1-r3,c1-c3) (r2-r3,c2-c3) = 0
    &&
    sign (r1-r3) = sign (r2-r1) && sign (c1-c3) = sign(c2-c1)
    
let rec ReorderByRotation k = function
    | [] -> []
    | last::[] -> [last]
    | a1::a2::[] -> [a1;a2]
    | a1::a2::tail when areSame k a1 a2 -> let (l1, l2) = (a2::tail) |> List.partition(areSame k a1) in a1::(ReorderByRotation k (l2@l1))
    //| a1::a2::tail when areSame k a1 (tail |> List.last) -> (a1::a2::tail)
    //| a1::a2::tail when areSame k a1 a2 -> ReorderByRotation k (a1::tail@[a2])
    | a::tail -> a::(ReorderByRotation k tail)

let OrderOfVaporization arr (x, y) =
    let crossProduct (x1,y1) (x2,y2) =
        let d1 = dist2(x, y)(x1,y1)
        let d2 = dist2(x, y)(x2,y2)
        match (cross(x1-x, y1-y)(x2-x,y2-y)) with
        | _ when y1 > y && y > y2 -> -1
        | _ when y1 < y && y < y2 -> +1
        | _ when y1 = y && x1 < x && y2 <> y -> -1
        | _ when y1 = y && x1 > x && y2 > y -> +1
        | _ when y1 = y && x1 > x && y2 < y -> -1
        | _ when y1 = y && y2 = y && x1 < x && x < x2 -> -1
        | _ when y1 = y && y2 = y && x1 > x && x > x2 -> +1
        | 0 when d1 < d2 -> -1
        | 0 when d1 > d2 -> +1
        | 0 -> failwithf "bad %d %d %d %d" x1 y1 x2 y2
        | pos when pos > 0 -> +1
        | _ -> -1

    arr |> Array.filter((<>)(x,y)) |> Array.toList |> List.sortWith crossProduct |> ReorderByRotation (x,y)

let observes = allAsteroids |> Array.map(fun (r,c) -> ((r,c),CountVisible allAsteroids (r,c))) |> Array.maxBy snd

// 31,25

OrderOfVaporization allAsteroids (31,25) |> List.skip 199


OrderOfVaporization [|(1,1);(1,0);(1,-1);(0,-1);(-1,-1);(-1,0);(-1,+1);(0,+1)|] (0,0)

OrderOfVaporization  [|(1,0);(0,1);(-1,0);(0,-1)|] (0,0)

0
module OrbitSimulator

// ==================== DOMAIN ==================== //
type [<Measure>] spaceUnit
type [<Measure>] spaceTime
type [<Measure>] Velocity = spaceUnit/spaceTime

type Space3D =
    {
        X: int<spaceUnit>;
        Y: int<spaceUnit>;
        Z: int<spaceUnit>;
    } with
    static member (+) (p1:Space3D,p2:Space3D) =
        {
            X = p1.X + p2.X
            Y = p1.Y + p2.Y
            Z = p1.Z + p2.Z
        }
    member this.energy =
        abs (int this.X) + abs (int this.Y) + abs (int this.Z)

type Velocity3D =
    {
        dX: int<Velocity>
        dY: int<Velocity>
        dZ: int<Velocity>
    } with
    static member (+) (v1:Velocity3D, v2:Velocity3D) =
        {
            dX = v1.dX + v2.dX
            dY = v1.dY + v2.dY
            dZ = v1.dZ + v2.dZ
        }
    static member (*) (me, scale:int<spaceTime>) =
        {
            X = me.dX * scale
            Y = me.dY * scale
            Z = me.dZ * scale
        }
    member this.energy =
        abs (int this.dX) + abs (int this.dY) + abs (int this.dZ)

let gravity : int<spaceUnit> * int<spaceUnit> -> int<Velocity> = function
    | (a, b) when int a < int b -> +1<Velocity>
    | (a, b) when int a > int b -> -1<Velocity>
    | _ -> 0<Velocity>

let (>=<) p1 p2 =
    {
        dX = gravity (p1.X, p2.X)
        dY = gravity (p1.Y, p2.Y)
        dZ = gravity (p1.Z, p2.Z)
    }

type Planet = Planet of Space3D * Velocity3D
let PlanetPosition = function | Planet(position, _) -> position
let PlanetVelocity = function | Planet(_, velocity) -> velocity
let PlanetEnergy = function | Planet(position, velocity) -> (position.energy) * (velocity.energy)

// ==================== /DOMAIN ==================== //

let fromString (str:string) =
    let [| X; Y; Z |] =
        str.TrimStart('<').TrimEnd('>').Replace(" ","").Split(',')
        |> Array.map(fun t -> Array.get (t.Split('=')) 1)
        |> Array.map(fun coord -> int coord * 1<spaceUnit>)

    Planet(
        { X = X; Y = Y; Z = Z},
        { dX = 0<Velocity>; dY = 0<Velocity>; dZ = 0<Velocity>}
    )

let applyGravity planets = function
    | Planet(myPosition, myVelocity) ->
        let newVelocity =
            planets
            |> Array.map PlanetPosition
            |> Array.map((>=<)(myPosition))
            |> Array.fold (+) myVelocity

        Planet(myPosition, newVelocity)
    
let movePlanet time = function
    | Planet(myPosition, myVelocity) -> Planet(myPosition + myVelocity * time, myVelocity)

let mutable mapX = Map.empty
let mutable mapY = Map.empty
let mutable mapZ = Map.empty

let stateDiff (mp:byref<_>) key value =
    match (mp |> Map.tryFind key) with
    | None -> mp <- mp |> Map.add key value
              None
    | Some t -> let res = value - t
                printfn "Found %A with %d %d" key t value
                mp <- mp |> Map.remove key |> Map.add key value
                Some(res)

let tryUpdate (mp:byref<_>) key value = function
    | None -> stateDiff &mp key value
    | res -> res

let simulateSpace timeStep ((globalTime, (allPlanets:Planet[]), (cycleX, cycleY, cycleZ)) as currentState) =
    let stateX =
        (int ((allPlanets.[0] |> PlanetPosition).X),
         int ((allPlanets.[1] |> PlanetPosition).X),
         int ((allPlanets.[2] |> PlanetPosition).X),
         int ((allPlanets.[3] |> PlanetPosition).X),
         int ((allPlanets.[0] |> PlanetVelocity).dX),
         int ((allPlanets.[1] |> PlanetVelocity).dX),
         int ((allPlanets.[2] |> PlanetVelocity).dX),
         int ((allPlanets.[3] |> PlanetVelocity).dX))

    let stateY =
        (int ((allPlanets.[0] |> PlanetPosition).Y),
         int ((allPlanets.[1] |> PlanetPosition).Y),
         int ((allPlanets.[2] |> PlanetPosition).Y),
         int ((allPlanets.[3] |> PlanetPosition).Y),
         int ((allPlanets.[0] |> PlanetVelocity).dY),
         int ((allPlanets.[1] |> PlanetVelocity).dY),
         int ((allPlanets.[2] |> PlanetVelocity).dY),
         int ((allPlanets.[3] |> PlanetVelocity).dY))

    let stateZ =
        (int ((allPlanets.[0] |> PlanetPosition).Z),
         int ((allPlanets.[1] |> PlanetPosition).Z),
         int ((allPlanets.[2] |> PlanetPosition).Z),
         int ((allPlanets.[3] |> PlanetPosition).Z),
         int ((allPlanets.[0] |> PlanetVelocity).dZ),
         int ((allPlanets.[1] |> PlanetVelocity).dZ),
         int ((allPlanets.[2] |> PlanetVelocity).dZ),
         int ((allPlanets.[3] |> PlanetVelocity).dZ))

    let tc = Array.copy allPlanets
    let movedPlanets1 = allPlanets |> Array.map(applyGravity tc)
    let movedPlanets = movedPlanets1 |> Array.map (movePlanet timeStep)

    let newStateX = tryUpdate &mapX stateX (int globalTime) cycleX
    let newStateY = tryUpdate &mapY stateY (int globalTime) cycleY
    let newStateZ = tryUpdate &mapZ stateZ (int globalTime) cycleZ

    let newSpaceState = (globalTime + timeStep, movedPlanets, (newStateX, newStateY, newStateZ))

    match (cycleX, cycleY, cycleZ) with
    | (Some a, Some b, Some c) -> printfn "%A %A %A" stateX stateY stateZ; None
    | _ -> Some(currentState, newSpaceState)

let planets =
    System.IO.File.ReadAllLines("C:\Users\olstakh\Source\Repos\AdventOfCode\AdventOfCode\Input.txt")
    |> Array.map fromString

let lastState =
    Seq.unfold(simulateSpace 1<spaceTime>) (0<spaceTime>, planets, (None,None,None))
    |> Seq.last

let (lastTimer, _, (dx,dy,dz)) = lastState


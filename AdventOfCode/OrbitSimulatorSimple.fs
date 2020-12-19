module OrbitSimulatorSimple

// ==================== DOMAIN ==================== //
type [<Measure>] spaceUnit
type [<Measure>] spaceTime
type [<Measure>] Velocity = spaceUnit/spaceTime

type Space1D = { X : int<spaceUnit> } with static member (+) (s1,s2) = { X = s1.X + s2.X }
type Velocity1D = { dX : int<Velocity> } with
    static member (+) (v1,v2) = { dX = v1.dX + v2.dX }
    static member (*) (v, scale:int<spaceTime>) = { X = v.dX * scale }

let gravity : int<spaceUnit> * int<spaceUnit> -> int<Velocity> = function
| (a, b) when int a < int b -> +1<Velocity>
| (a, b) when int a > int b -> -1<Velocity>
| _ -> 0<Velocity>

let (>=<) p1 p2 =
    {
        dX = gravity (p1.X, p2.X)
    }

type Planet1D = Planet1D of Space1D * Velocity1D
let PlanetPosition = function | Planet1D(position, _) -> position
let PlanetVelocity = function | Planet1D(_, velocity) -> velocity

type Planet =
    {
        PlanetX : Planet1D
        PlanetY : Planet1D
        PlanetZ : Planet1D
    }
let BreakPlanet p = (p.PlanetX, p.PlanetY, p.PlanetZ)

// ==================== /DOMAIN ==================== //

let fromString (str:string) =
    let [| X; Y; Z |] =
        str.TrimStart('<').TrimEnd('>').Replace(" ","").Split(',')
        |> Array.map(fun t -> Array.get (t.Split('=')) 1)
        |> Array.map(fun coord -> int coord * 1<spaceUnit>)

    {
        PlanetX = Planet1D({X = X}, {dX = 0<Velocity>})
        PlanetY = Planet1D({X = Y}, {dX = 0<Velocity>})
        PlanetZ = Planet1D({X = Z}, {dX = 0<Velocity>})
    }

let applyGravity planets = function
    | Planet1D(myPosition, myVelocity) ->
        let newVelocity =
            planets
            |> Array.map PlanetPosition
            |> Array.map((>=<)(myPosition))
            |> Array.fold (+) myVelocity

        Planet1D(myPosition, newVelocity)

let movePlanet time = function
    | Planet1D(myPosition, myVelocity) -> Planet1D(myPosition + myVelocity * time, myVelocity)

let simulate timeStep ((globalTime, allPlanets, savedStates) as currentState) =
    let movedPlanets = allPlanets |> Array.map(applyGravity allPlanets) |> Array.map(movePlanet timeStep)

    match (savedStates |> Map.tryFind allPlanets) with
    | None -> let newState = (globalTime + timeStep, movedPlanets, savedStates |> Map.add allPlanets (int globalTime))
              Some(currentState, newState)
    | Some _ -> None

let (planetsX, planetsY, planetsZ) = 
    System.IO.File.ReadAllLines("C:\Users\olstakh\Source\Repos\AdventOfCode\AdventOfCode\Input.txt")
    |> Array.map fromString
    |> Array.map BreakPlanet
    |> Array.unzip3

let (xTimer, xPlanets, xMap) = Seq.unfold (simulate 1<spaceTime>) (0<spaceTime>, planetsX, Map.empty) |> Seq.last
let (yTimer, yPlanets, yMap) = Seq.unfold (simulate 1<spaceTime>) (0<spaceTime>, planetsY, Map.empty) |> Seq.last
let (zTimer, zPlanets, zMap) = Seq.unfold (simulate 1<spaceTime>) (0<spaceTime>, planetsZ, Map.empty) |> Seq.last

// 1014, 1966, 2351
// 3932

// 96236, 286332, 186028 -> 4 

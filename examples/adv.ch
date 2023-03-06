import

"lib/prelude.ch" :: ""
"lib/strings.ch"

def

Location = struct(description, N, S, E, W string)

GameState = struct(locations map, playerLocation, output string)

var

state = GameState(map(), "", "")

cmd

main :
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
given :
    linesToProcess = (file "examples/locations.rsc")[contents]

def

parseUserInput(input string) :
    input >> strings.toUpper >> strings.s

slurpLocations(L list): 
    (while unfinished do getLocation to 0, map())[1]
given :
    unfinished(counter, locs) : counter < len(L)
    getLocation(counter, locs) : counter + 6, locs with L[counter]:: ..
        .. Location(L[counter + 1], L[counter + 2], L[counter + 3], L[counter + 4], L[counter + 5])

describe(S GameState) : S[playerLocation] + "\n\n" + S[locations][S[playerLocation]][description]

DIRECTIONS = map("NORTH"::N, "SOUTH"::S, "EAST"::E, "WEST"::W)

doMove(dir string, S GameState) :
    newLocation == "" :
        error "you can't go that way!"
    else :
        S with playerLocation::newLocation
given :
    directionFromString = DIRECTIONS[dir]
    newLocation = S[locations][S[playerLocation]][directionFromString]

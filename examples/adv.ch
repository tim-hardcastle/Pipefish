import

"lib/strings.ch"

def

Location = struct(description, north, south, east, west string)
Object = struct(description, location string)
GameState = struct(locations, objects map, playerLocation, output string)

var

state = GameState(map(), map(), "", "")

cmd

main :
    get linesToProcess from File "examples/locations.rsc", list
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
    get moreLinesToProcess from File "examples/objects.rsc", list
    state = state with objects::slurpObjects(moreLinesToProcess)
    post "\n" + describe(state[playerLocation], state) + "\n\n" to Output()
    loop :
        get userInput from Input "What now? "
        strings.toLower(userInput) == "quit" :
            break
        else :
            state = doTheThing(userInput, state)
            post "\n" + state[output] + "\n" to Output()
   
def

VERBS = {"go", "take", "drop", "examine", "inventory", "look"}
INTRANSITIVE_VERBS = {"inventory", "look"}

// doTheThing selects which function to call based on the parsed user input.

doTheThing(input string, S GameState) :
    not (len(parsedInput) in {1, 2}) :
        S with output::"I don't understand that."
    not verb in VERBS :
        S with output::"I don't know the word " + verb + "."
    not verb in INTRANSITIVE_VERBS and len(parsedInput) == 1 :
        S with output::"The verb " + verb + " requires a noun."
    verb in INTRANSITIVE_VERBS and len(parsedInput) == 2 :
        S with output::"The verb " + verb + " requires no noun."
    specialEffects(parsedInput, S) 
    verb == "go" :
        doMove(noun, S)
    verb == "look" :
        doLook(S)
    verb == "take" :
        doTake(noun, S)
    verb == "drop" :
        doDrop(noun, S)
    verb == "examine" :
        doExamine(noun, S)
    verb == "inventory" :
        doInventory(S)
    else :
        S with output::"I don't know how to do that!"
given :
    parsedInput = parseUserInput(input)
    verb = parsedInput[0]
    noun = parsedInput[1]

specialEffects(parsedInput, S) :
    verb == "take" and noun == "cat" :
        S with output::"The cat eludes your grip."
    verb == "drop" and noun == "ring" and S[playerLocation] == "The brink of an active volcano" :
        S with [objects, "ring", location]::"Limbo", output::"You cast the magical ring into the volcano. Oh, that's original."
given :
    verb = parsedInput[0]
    noun = parsedInput[1]

// Parsing input.

parseUserInput(input string) :
    input -> strings.toLower -> strings.split(that, " ") >> substituteSynonyms -> addImplicitGo

SYNONYMS = map("get"::"take", "inv"::"inventory", "ex"::"examine", "n"::"north", 
            .. "s"::"south", "e"::"east", "w"::"west")

substituteSynonyms(s string) :
    s in keys SYNONYMS :
        SYNONYMS[s]
    else :
        s

addImplicitGo(L list) :
    len(L) == 1 and L[0] in keys DIRECTIONS:
        ["go"] + L
    else :
        L

// Functions for describing a location and its contents.

describe(loc string, S GameState) : loc + "\n\n" + S[locations][loc][description] ..
                                 .. + describeObjects(loc, S)

describeObjects(loc, S) :
    objectsPresent == [] :
        ""
    else :
        "\n\nThere is" + describeList(objectsPresent) + " here."
given :
    objectsPresent = ((keys S[objects]) ?> S[objects][that][location] == loc)

describeList(L list) :
    (while unfinished do addToString to (0, ""))[1]
given :
    unfinished(counter, total) : counter < len L
    addToString(counter, total) :
        counter > 0 and counter < len(L) - 1 : 
            counter + 1, total + "," + nounWithArticle
        counter > 0 and counter == len(L) - 1 : 
            counter > 1 :
                counter + 1, total + ", and" + nounWithArticle
            else :
                counter + 1, total + " and" + nounWithArticle
        else :
            counter + 1, total + nounWithArticle
    given :
        nounWithArticle = addIndefiniteArticle(L[counter])

addIndefiniteArticle(s string) :
    s[0] in {"a", "e", "i", "o", "u"} :
        " an " + s
    else :
        " a " + s

// Functions for executing the end-user's instructions.

DIRECTIONS = map("north"::north, "south"::south, "east"::east, "west"::west)

doMove(dir string, S GameState) : 
    not dir in keys DIRECTIONS :
        S with output::"That's not a direction!"
    newLocation == "" : 
        S with output::"You can't go that way!"
    else :
        S with playerLocation::newLocation, output::describe(newLocation, S)
given :
    directionFromString = DIRECTIONS[dir] 
    newLocation = S[locations][S[playerLocation]][directionFromString]

doTake(obj string, S GameState) : 
    not obj in keys S[objects] : 
        S with output::"I don't know what that is."
    not S[objects][obj][location] == S[playerLocation] :
        S with output::"I don't see that here."
    else :
        S with [objects, obj, location]::"Player", output::"You take the " + obj + "."

doDrop(obj string, S GameState) : 
    not obj in keys S[objects] : 
        S with output::"I don't know what that is."
    not S[objects][obj][location] == "Player" :
        S with output::"You don't have that."
    else :
        S with [objects, obj, location]::S[playerLocation], output::"You drop the " + obj + "."

doExamine(obj string, S GameState) : 
    not obj in keys S[objects] : 
        S with output::"I don't know what that is."
    not S[objects][obj][location] in {S[playerLocation], "Player"} :
        S with output::"I don't see that here."
    else :
        S with output::S[objects][obj][description]

doInventory(S GameState) :
    objectsPlayerIsCarrying == [] :
        S with output::"You aren't carrying anything."
    else :
        S with output::"You have" + describeList(objectsPlayerIsCarrying) + "."
given :
    objectsPlayerIsCarrying = ((keys S[objects]) ?> S[objects][that][location] == "Player")

doLook(S GameState) :
    S with output::describe(S[playerLocation], S)

// Data slurpers.

slurpLocations(L list): 
    (while unfinished do getLocation to 0, map())[1]
given :
    unfinished(counter, locs) : counter < len(L)
    getLocation(counter, locs) : counter + 6, locs with L[counter]:: ..
        .. Location(L[counter + 1], L[counter + 2], L[counter + 3], L[counter + 4], L[counter + 5])
    
slurpObjects(L list): 
    (while unfinished do getObject to 0, map())[1]
given :
    unfinished(counter, locs) : counter < len(L)
    getObject(counter, locs) : counter + 3, locs with L[counter]:: ..
        .. Object(L[counter + 1], L[counter + 2])
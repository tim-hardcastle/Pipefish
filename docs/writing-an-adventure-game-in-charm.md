# Writing an adventure game in Charm.

This documents demonstrates the Charm language by showing how to use it to write a simple text-based adventure game. It assumes that you have no familiarity with Charm, but that you would be able to easily follow a tutorial on this subject if it was presented in your own favorite language instead.

So, let’s write an adventure game in Charm!

A location in the game should have a short heading, a long description, and then details of the exits, if any, north, south, east, or west. We'll keep this in a flat data file to be read on initialization, so that subsequent lines will be the heading, description, north, south, east, and west exits respectively. The particular file we'll be using is here.

First, we will want some data structures. Because here we’re defining constant features of the code, we put them under the heading `def`, like so:

```
def

Location = struct(description, NORTH, SOUTH, EAST, WEST string)
GameState = struct(locations map, playerLocation, output string)
```

We will add to the GameState structure later so that we can talk about game objects: this is just a beginning.

So now we want to slurp those undifferentiated lines from our flat file into our data structure, for which we'll want a `while` loop. First we need to import the `while` loop at the top of the script, with:

```
import

"lib/prelude.ch" :: ""
```

This imports `while` and other useful functions into an empty namespace. Now let's see what it does! If this is your first functional programming language, this is where you're going to see something new. In an imperative language, we go round and round a loop changing the variables we're interested in until some condition is met. It is literally impossible to do this in Charm. Let's look at what we do instead.

The `while` construct in Charm is a function with the signature `while (condition func) do (action func) to (data tuple)`, where `condition` must return a boolean. The `while` function returns the thing we get if we keep applying `action` to `data` until `condition(data)` is `false`.

This will become much clearer with a few examples. Let's write a function that adds up the numbers from 1 to 4. In Charm functions, the `given` section at the bottom contains definitions of local functions and constants.

```
def

tenpins :
	while condition do action to (1, 0)
given :
	condition(counter, total) : counter <= 4
	action(counter, total) : counter + 1, total + counter
```

So, this gives us what we get if we go on adding 1 to a counter starting at 1, and we go on adding the counter to a total starting at 0, while the counter is less than or equal to 4. This will return `10`.

Or rather, it will return the tuple `5, 10`, since it will return the final value of the counter and the total both, so we should modify our function like this, just as we do in Python when we want to get only one of a function's return values:

```
tenpins :
	(while condition do action to (1, 0))[1]
given :
	condition(counter, total) : counter <= 4
	action(counter, total) : counter + 1, total + counter
```

Charm is 0-indexed, so that will return just the value we're using as the total.

Now, suppose we wanted our function to depend on a parameter, so we could add up the numbers from 1 to `n` inclusive:

```
triangularNumber(n) :
	(while condition do action to (1, 0))[1]
given :
	condition(counter, total) : counter <= n
	action(counter, total) : counter + 1, total + counter
```

The point here is that `n` doesn't need to be passed to the local functions: they are closures, they can see the parameters of the outer function.

On this basis, it's easy to see how to slurp our data out of the list.

```
slurpLocations(L list): 
    (while unfinished do getLocation to 0, map())[1]
given :
    unfinished(counter, locs) : counter < len(L)
    getLocation(counter, locs) : counter + 6, locs with L[counter]:: ..
        .. Location(L[counter + 1], L[counter + 2], L[counter + 3], L[counter + 4], L[counter + 5])
```

But we've only written the function. We haven't made anything happen!

In Charm, things that actually make stuff happen are special, and are written under the headword `cmd`. In this case, we want to first of all get the data from the flat file, then shove it through our slurpLocations function, and then assign it to something.

So, we'll need something to assign it *to*: we need to declare a variable. This is done in the `var` section of the script.

```
var

state = GameState(map(), map(), "", "")
```

And then in the `main` command, which is always executed on initialization if it exists, we can put this:

```
main :
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
given :
    linesToProcess = (file "examples/locations.rsc")[contents]
```

(For convenience, we can always start the player off in the first location on our list, hence `playerLocation::linesToProcess[0]`)

Our script now looks like this:

```
import

"lib/prelude.ch" :: ""

def

Location = struct(description, NORTH, SOUTH, EAST, WEST string)
GameState = struct(locations map, playerLocation, output string)

var

state = GameState(map(), "", "")

cmd

main :
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
given :
    linesToProcess = (file "examples/locations.rsc")[contents]

def

slurpLocations(L list): 
    (while unfinished do getLocation to 0, map())[1]
given :
    unfinished(counter, locs) : counter < len(L)
    getLocation(counter, locs) : counter + 6, locs with L[counter]:: ..
        .. Location(L[counter + 1], L[counter + 2], L[counter + 3], L[counter + 4], L[counter + 5])
```

Let's run it in the REPL and see if it works so far:


Let's add a little function to describe a location.

```
describe(loc string, S GameState) : loc + "\n\n" + S[locations][loc][description]

```

Now, how about moving around the map? Let's make a function which returns the game state you'd get if the player moved in a particular direction. We'll use a map to relate the strings "north", "south", "east" and "west" to the keys of the Location struct.

```
DIRECTIONS = map("north"::NORTH, "south"::SOUTH, "east"::EAST, "west"::WEST)

doMove(dir string, S GameState) : 
    not noun in keys DIRECTIONS :
        S with output::"That's not a direction!"
    newLocation == "" : 
        S with output::"You can't go that way!"
    else :
        S with playerLocation::newLocation, output::describe(newLocation, S)
given :
    directionFromString = DIRECTIONS[dir] 
    newLocation = S[locations][S[playerLocation]][directionFromString]
```

Try it out in the REPL:

```
ADV → describe((doMove("north", state))[playerLocation], state)                                                                                 
The wizard's hall

You are in the antechamber of the wizard's castle. To your south is the outdoors and the castle gardens. To your west, a door stands ajar through which you catch a glimpse of shelving and leather-bound books: presumably a library. To the north an archway gives a view of a banqueting hall. A low mean passage leads east.   
ADV →
```

It works. However, `describe((doMove("north", state))[playerLocation], state)` is an ugly mess of parentheses, and the sort of thing that used to get functional programming a bad name, so this might be a good time to introduce Charm's streaming operators. In particular, the piping operator lets us write expressions like the above from left to right instead.

```
ADV → state >> doMove("north", that) >> that[playerLocation] >> describe(that, state)                                                                      
The wizard's hall

You are in the antechamber of the wizard's castle. To your south is the outdoors and the castle gardens. To your west, a door stands ajar through which you catch a glimpse of shelving and leather-bound books: presumably a library. To the north an archway gives a view of a banqueting hall. A low mean passage leads east.  
ADV →
```

The piped expression means just the same thing as describe((doMove("north", state))[playerLocation], state) , but describes it in the form of a pipeline where `that` refers to whatever's to the left of the piping operator `>>`.

This becomes more and more useful the more complicated our expressions become. No-one wants to read or write `describe((doMove("east", doMove("north", state)))[playerLocation], state)`, but this becomes perfectly lucid when written with the piping operator as:

```
state >> doMove("north", that) >> doMove("east", that) >> that[playerLocation] >> describe(that, state)   
```

As a final piece of syntactic sugar, the word `that` is unnecessary when the function has only one parameter.

Note that like every other function, doMove *doesn't change the data it's given*.

```
ADV → describe state[playerLocation], state 
The wizard's garden

You are in a garden filled with magical flowers: Mandrake, Warlock's Henbane, Speaking Upas, and the like. To your north is the entryway to the wizard's castle: a great door stands open.  
ADV →                
```

To actually change the state variable, we'd have to do something imperative to it: we'd have to write an assignment in the `cmd` section saying `state = <something>`. Eventually we will, but we're not there yet.

First, let's make a little parser for user input. This is a very simple game, so we will just have the verbs "go", "take", "drop", "examine", "inventory", and "look". Nouns will be the objects in the game plus the four directions. We will allow "n", "s", "e", "w", "get", "ex", and "inv" as synonyms for "north", "south", "east", "west", "take", "examine" and "inventory" respectively, and we will allow any of the four directions to be used on its own with `go` implied. Input will be case-insensitive.

At the very top of our script we need to add an `import` section to get the `strings` library. Then in the `def` section we can write a parsing function. It can be reassuring to write such functions a bit at a time, as I'll demonstrate here:

```
parseUserInput(input string) :
    input >> strings.toLower >> strings.split(that, " ")
```

In the REPL:

```
ADV → parseUserInput "tAkE SWORD" 
[take, sword]
```

OK so far! Now let's add a map of synonyms and a little function to substitute them:

```
SYNONYMS = map("get"::"take", "inv"::"inventory", "ex"::"examine", "n"::"north", 
            .. "s"::"south", "e"::"east", "w"::"west")

substituteSynonyms(s string) :
    s in keys SYNONYMS :
        SYNONYMS[s]
    else :
        s
```

Now we can add that to our parsing pipeline using the mapping operator `]>`. This will apply the function to each member of our list of words one at a time:

```
parseUserInput(input string) :
    input >> strings.toLower >> strings.split(that, " ") ]> substituteSynonyms
```

Let's check it out in the REPL:

```
ADV → parseUserInput "gEt SwOrd" 
[take, sword]
ADV →     
```

Finally, let's put in a thing that turns "north" into "go north", etc:

```
addImplicitGo(L list) :
    len(L) == 1 and L[0] in keys DIRECTIONS:
        ["go"] + L
    else :
        L
```

... and plug it into our pipeline:

```
parseUserInput(input string) :
    input >> strings.toLower >> strings.split(that, " ") ]> substituteSynonyms >> addImplicitGo
```

In the REPL:

```
ADV → parseUserInput "N" 
[go, north]
ADV →   
```

Now let's move on to --- moving on! We're so close to being able to move around the map. We will need a function that tells us what we get given the user input and the state:

```
VERBS = {"go", "take", "drop", "examine", "inventory", "look"}
INTRANSITIVE_VERBS = {"inventory", "look"}

doTheThing(input string, S GameState) :
    not (len(parsedInput) in {1, 2}) :
        S with output::"I don't understand that."
    not verb in VERBS :
        S with output::"I don't know the word " + verb + "."
    not verb in INTRANSITIVE_VERBS and len(parsedInput) == 1 :
        S with output::"The verb " + verb + " requires a noun."
    verb in INTRANSITIVE_VERBS and len(parsedInput) == 2 :
        S with output::"The verb " + verb + " requires no noun."
    verb == "go" :
        doMove(noun, S)
    else :
        S with output::"I don't know how to do that!"
given :
    parsedInput = parseUserInput(input)
    verb = parsedInput[0]
    noun = parsedInput[1]
```

Note that it doesn't matter if we get errors in the `given` section when we assign `noun` and `verb` --- if `parsedInput` is an empty list, error values will indeed be assigned to these local constants, but as you can see from the logic of the function, they would then never be used, so the errors remain trapped in the local variables, unable to crash their way up the stack.

Check it out in the REPL:

```
ADV → doTheThing("go north", state) >> that[output]    
                                                                                                     
The wizard's hall

You are in the antechamber of the wizard's castle. To your south is the outdoors and the castle gardens. To your west, a door stands ajar through which you catch a glimpse of shelving and leather-bound books: presumably a library. To the north an archway gives a view of a banqueting hall. A low mean passage leads east. 

ADV → 
```

At this point we can make a little "inner REPL" for the game itself, and take a stroll around our map. Here's what it looks like: it must go in the `cmd` section because it is aggressively imperative: the keywords `request` and `respond` can be used nowhere except the `cmd` section.

```
repl :
    strings.toLower(userInput) == "quit" :
        break
    else :
        state = doTheThing(userInput, state)
        respond state[output]
given :
    userInput = request "What now? "
```

You may be wondering where the loop is in this `repl` command. It's implicit --- if a command has a `request` for user input in it, and if when called its path of execution leads to that `request`, then Charm will automatically loop over the command until (a) it executes without reaching the `request`, (b) it reaches `break`, which stops the loop, or `stop`, which stops the whole Charm service.

And we also need to add a line in the `main` cmd calling the `repl` cmd. We'll throw in a line describing the initial location when you start the game, since that shouldn't be in a loop.

```
main :
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
    respond describe(state[playerLocation], state)
    repl
given :
    linesToProcess = (file "examples/locations.rsc")[contents] 
```

At this point, our script looks like this:

```
import

"lib/prelude.ch" :: ""
"lib/strings.ch"

def

Location = struct(description, NORTH, SOUTH, EAST, WEST string)
GameState = struct(locations map, playerLocation, output string)

var

state = GameState(map(), "", "")

cmd

main :
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
    respond  "\n" + describe(state[playerLocation], state) + "\n\n"
    repl
given :
    linesToProcess = (file "examples/locations.rsc")[contents] 

repl :
    strings.toLower(userInput) == "quit" :
        break
    else :
        state = doTheThing(userInput, state)
        respond "\n" + state[output] + "\n\n"
given :
    userInput = request "What now? "
   
def

VERBS = {"go", "take", "drop", "examine", "inventory", "look"}
INTRANSITIVE_VERBS = {"inventory", "look"}

doTheThing(input string, S GameState) :
    not (len(parsedInput) in {1, 2}) :
        S with output::"I don't understand that."
    not verb in VERBS :
        S with output::"I don't know the word " + verb + "."
    not verb in INTRANSITIVE_VERBS and len(parsedInput) == 1 :
        S with output::"The verb " + verb + " requires a noun."
    verb in INTRANSITIVE_VERBS and len(parsedInput) == 2 :
        S with output::"The verb " + verb + " requires no noun."
    verb == "go" :
        doMove(noun, S)
    else :
        S with output::"I don't know how to do that!"
given :
    parsedInput = parseUserInput(input)
    verb = parsedInput[0]
    noun = parsedInput[1]

parseUserInput(input string) :
    input >> strings.toLower >> strings.split(that, " ") ]> substituteSynonyms >> addImplicitGo

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

describe(loc string, S GameState) : loc + "\n\n" + S[locations][loc][description]

DIRECTIONS = map("north"::N, "south"::S, "east"::E, "west"::W)

doMove(dir string, S GameState) : 
    not noun in keys DIRECTIONS :
        S with output::"That's not a direction!"
    newLocation == "" : 
        S with output::"You can't go that way!"
    else :
        S with playerLocation::newLocation, output::describe(newLocation, S)
given :
    directionFromString = DIRECTIONS[dir] 
    newLocation = S[locations][S[playerLocation]][directionFromString]

slurpLocations(L list): 
    (while unfinished do getLocation to 0, map())[1]
given :
    unfinished(counter, locs) : counter < len(L)
    getLocation(counter, locs) : counter + 6, locs with L[counter]:: ..
        .. Location(L[counter + 1], L[counter + 2], L[counter + 3], L[counter + 4], L[counter + 5])
```

Run the script in the REPL:

```                                                                                                                                                     
ADV → main                

The wizard's garden

You are in a garden filled with magical flowers: Mandrake, Warlock's Henbane, Speaking Upas, and the like. To your north is the entryway to the wizard's castle: a great door stands open.

What now? n

The wizard's hall

You are in the antechamber of the wizard's castle. To your south is the outdoors and the castle gardens. To your west, a door stands ajar through which you catch a glimpse of shelving and leather-bound books: presumably a library. To the north an archway gives a view of a banqueting hall. A low mean passage leads east.

What now? e

The kitchen

You are in an old-fashioned kitchen with turning-spits and whatnot. Eye of newt and toe of frog bubble cheerfully in the cauldron. To your east is the kitchen garden.

What now? e                 

The kitchen garden

This is a relatively normal kitchen garden, because wizards like a potato the same as the rest of us. To your west is the door into the kitchen, and to your north a doorway in the garden wall leads to the brink of an active volcano.

What now? s              

You can't go that way!

What now? w

The kitchen

You are in an old-fashioned kitchen with turning-spits and whatnot. Eye of newt and toe of frog bubble cheerfully in the cauldron. To your east is the kitchen garden.

What now? quit              
ADV →    
```

We have a tiny adventure! Great!

Having established that that works, let's immediately comment out that line in `main` that calls `repl`. Why? Because it's far more convenient for us to go on developing by poking at our code through Charm's own native REPL. This Is The Way. We'll comment out the line that describes the initial location too, since it will be annoying otherwise.

Let's add some objects. Our flat data file will list them as successive lines of name of object, description of object, and initial location of object.

We add more slurping logic to the `def` section ...

```
slurpObjects(L list): 
    (while unfinished do getObject to 0, map())[1]
given :
    unfinished(counter, locs) : counter < len(L)
    getObject(counter, locs) : counter + 3, locs with L[counter]:: ..
        .. Object(L[counter + 1], L[counter + 2])
```

... and the actual slurping to the `main` cmd.

```
main :
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
    state = state with objects::slurpObjects(moreLinesToProcess)
    // respond  "\n" + describe(state[playerLocation], state) + "\n\n"
    // repl
given :
    linesToProcess = (file "examples/locations.rsc")[contents] 
    moreLinesToProcess = (file "examples/objects.rsc")[contents] 
```

Now let's update our `describe` function. We'll give it some helper functions to cope with the distinction between "a" and "an" in English, the Oxford comma, etc. This is our most deeply nested function!

```
describeObjects(loc, Gamestate) :
    objectsPresent == [] :
        ""
    else :
        "\n\nHere there is " + describeList(objectsPresent)
given :
    objectsPresent = (keys state[objects]) ?> state[objects][that][location] == loc

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
```

Note the use of the filter operator `?>` in `descibeObjects`.

Check it out in the REPL :

```
ADV → describe("The wizard's garden", state)
The wizard's garden

You are in a garden filled with magical flowers: Mandrake, Warlock's Henbane, Speaking Upas, and the like. To your north is the entryway to the wizard's castle: a great door stands open.

There is a ring here
ADV →          
```

We'll want a bunch of little functions analogous to doGo:

```
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
```

And we'll update doTheThing to call them under the appropriate circumstances.

```
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
```

Let's uncomment the call to `repl` and play our game!

```
ADV → main                

The wizard's garden

You are in a garden filled with magical flowers: Mandrake, Warlock's Henbane, Speaking Upas, and the like. To your north is the entryway to the wizard's castle: a great door stands open.

There is a ring here.

What now? take ring      

You take the ring.

What now? look            

The wizard's garden

You are in a garden filled with magical flowers: Mandrake, Warlock's Henbane, Speaking Upas, and the like. To your north is the entryway to the wizard's castle: a great door stands open.

What now? inv             

You have a ring.

What now? go north         

The wizard's hall

You are in the antechamber of the wizard's castle. To your south is the outdoors and the castle gardens. To your west, a door stands ajar through which you catch a glimpse of shelving and leather-bound books: presumably a library. To the north an archway gives a view of a banqueting hall. A low mean passage leads east.

What now? east              

The kitchen

You are in an old-fashioned kitchen with turning-spits and whatnot. Eye of newt and toe of frog bubble cheerfully in the cauldron. To your east is the kitchen garden.

There is an egg here.

What now? drop ring        

You drop the ring.

What now? look             

The kitchen

You are in an old-fashioned kitchen with turning-spits and whatnot. Eye of newt and toe of frog bubble cheerfully in the cauldron. To your east is the kitchen garden.

There is a ring and an egg here.

What now? examine egg       

The egg is egg-shaped and egg-colored. It's an egg.

What now? quit 
ADV → 
```

That gives us a standard framework for moving about, and for interacting with objects. However, in a real adventure game the puzzles hinge on certain actions triggering non-standard responses.

This gives me a chance to introduce one of the few (AFAIK) unique pieces of Charm syntax/semantics, the unsatisfied conditional. In the `doTheThing` method, after validating the parsed input and just before the line `verb == "go" :` we will instert the line `specialEffects(verb, noun, S)`

Here's the code for `specialEffects`.

```
specialEffects(verb, noun string, S GameState) :
    verb == "take" and noun == "cat" :
        S with output::"The cat eludes your grip."
    verb == "drop" and noun == "ring" and S[playerLocation] == "The brink of an active volcano" :
        S with [objects, "ring", location]::"Limbo", output "You cast the magical ring into the volcano. Oh, that's original."
```

You will notice that unlike all the other chains of conditionals we've used, it has no `else` at the end. Instead, if it doesn't return a value, `doTheThing` will continue on down its own chain of conditionals.

This is all we need for a simple adventure game (except a plot and a point). Let us look at the entire script. It is delightfully small.

```
import

"lib/prelude.ch" :: ""
"lib/strings.ch"

def

Location = struct(description, NORTH, SOUTH, EAST, WEST string)
Object = struct(description, location string)
GameState = struct(locations, objects map, playerLocation, output string)

var

state = GameState(map(), map(), "", "")

cmd

main :
    state = state with locations::slurpLocations(linesToProcess), playerLocation::linesToProcess[0]
    state = state with objects::slurpObjects(moreLinesToProcess)
    respond  "\n" + describe(state[playerLocation], state) + "\n\n"
    repl
given :
    linesToProcess = (file "examples/locations.rsc")[contents] 
    moreLinesToProcess = (file "examples/objects.rsc")[contents] 

repl :
    strings.toLower(userInput) == "quit" :
        break
    else :
        state = doTheThing(userInput, state)
        respond "\n" + state[output] + "\n\n"
given :
    userInput = request "What now? "
   
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
    input >> strings.toLower >> strings.split(that, " ") ]> substituteSynonyms >> addImplicitGo

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

DIRECTIONS = map("north"::NORTH, "south"::SOUTH, "east"::EAST, "west"::WEST)

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
```

So, we've developed a little app in Charm. Hopefully you've learned some Charm. Hopefully you've also learned something about functional programming.

When people talk about the merits of functional programming, they often in fact cry up the particular features of their favorite languages, features such as pattern-matching (if they use ML) or a highly-expressive type system (Haskell) or homoiconicity (Lisp). Charm has none of these things, but it does demonstrate the chief merit of functional programming, which is that it only has one design pattern: The Pipeline --- a pipeline which gradually tranforms your data though a series of small, easily-understood steps composed of functions which are small, shallow, trivial to understand, and easy to compose. Besides all the mere conveniences of this or that particular language, this I think is the essential value of functional programming.


import

"lib/strings.ch"::""

contacts

Bar::"path"

cmd

getTheThing(s) :
    get x from Bar --- returnTheThing |s|
    post "* " + x + " *" to Output()

makeBarDoTheThing(s) : 
    post Bar --- doTheThing |s|

import

gocode "regexp"

def

match(pattern, text string) : gocode {
	regObj, err := regexp.Compile(pattern)
	if err != nil {
		return err
	}
	return regObj.MatchString(text)
}

find(pattern, text string) : gocode {
	regObj, err := regexp.Compile(pattern)
	if err != nil {
		return err
	}
	return regObj.FindString(text)
}

findAll(pattern, text string) : gocode {
	regObj, err := regexp.Compile(pattern)
	if err != nil {
		return err
	}
	return regObj.FindAllString(text, -1)
}

findAllIndex(pattern, text string) : gocode {
	regObj, err := regexp.Compile(pattern)
	if err != nil {
		return err
	}
	indices := regObj.FindAllStringIndex(text, -1)
	resultList := &object.List{Elements: []object.Object{}}
	for _, ilist := range(indices) {
		resultList.Elements = append(resultList.Elements, &object.Pair{Left: &object.Integer{Value: ilist[0]}, Right: &object.Integer{Value: ilist[1]}})
	}
    return resultList
}
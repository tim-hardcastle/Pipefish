In order for the type system of an external Pipefish service to mesh with our own, we need to import its API and build a facsimile of its public types and type signatures.

We use the following format:

NAMESPACE | namespaceName
< All the public info of the namespace. >
END NAMESPACE

We put the namespaces at the top of the file because that's the order we should start them up in, as usual, dependencies first.

ENUM | enumName | ELEMENT1 ELEMENT2 ...

STRUCT | structName | fieldName1 type1a type1b ... |  fieldName2 type2a ...

ABSTRACT | abstractName | type1 type2 ...

FUNCTION | functionName | 0, 1, 2 for prefix/infix/postfix | parameterName1 type1 | parameterName2 type2 | serialization of typescheme

COMMAND : same as for a function.

Note that for the functions and commands the return types are not those supplied by the user but those inferred by the compiler.

We can then textually mung that into a library which declares the types, and has the body of each function/command be a call to the external service.

We protect ourselves from injection attacks by passing no piece of code longer than an identifier.

Typeschemes are serialized using reverse Polish notation.
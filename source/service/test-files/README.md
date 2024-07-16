To test the 'service' package properly, we need some scripts to initialize services from.

These are them. Do not remove.

The parser is tested here because unless you start up a service the builtins don't get slurped in and there's not much behavior to test. It has associated files so that we can test the parsing of user-defined suffixes and articles of bling that aren't exhibited in the builtins file.

The tests use a test helper which builds a service and then takes a function from a compiler and a string to a string, so it can flexibly test the output of the parser, or the parser errors, or values from the service, or the compiler errors, or input and posts to output, etc.
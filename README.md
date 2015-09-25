This is a just some code extracted from a one-off project I was tinkering with a while 
back, wherein I was attempting to reverse-engineer a proprietary 3d model format (why?
because I could, mostly, haha)

Basically, this is a binary data parser like Binary (which it uses for the fiddly details) 
but intended to proces a large, structured file as various semi-independent chunks. To 
help with the reverse-engineering process, I added some debugging output which turned out
to be really nice--by labelling parsers as fields or structs, it collects that information
and merges it together, then at the end prints out a hexdump of the input file chunked
up and annotated with what the named parsers did.

Since the file format was structured as small structures referencing each other via 
relative offsets, this annotated hexdump let me skim the file to see what parts had
been parsed by what and look for gaps that no parser reached.

This was particularly useful since if I found a large section of non-null bytes I could
look search the file for anything that could be an offset into that section. This let me
identify the high-level structure of the file much faster even when I had no idea yet what
a particular section meant!

I'm uploading it here at jwiegley's request since he thought the idea was interesting and
the annotated hexdump looked handy. If anyone wants to turn this pile of dubious hacks 
into a real library I'd love to see it. (And will probably help out, I just don't feel 
like starting the process right now...)

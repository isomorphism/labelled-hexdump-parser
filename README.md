This is just some code extracted from a one-off project I was tinkering with a while 
back, wherein I was attempting to reverse-engineer a proprietary 3d model format (why?
because I could, mostly, haha)

Basically, this is a binary data parser like Binary (which it uses for the fiddly details) 
but intended to proces a large, structured file as various semi-independent chunks. To 
help with the reverse-engineering process, I added some debugging output which turned out
to be really nice--by labelling parsers as fields or structs, it collects that information
and merges it together, then at the end prints out a hexdump of the input file chunked
up and annotated with what the named parsers did.

Here's a sample!
    
    ----------------------------------------------------------------------
    vertex group
    0x00004394 - 0x000043c3
    0x4394 flags: 1073741826
    0x4398 (16 bytes skipped): 
    0x43a8 vertex array bytes array, # entries: 135800
    0x43ac vertex array bytes array offset: 0x47178
    0x43b0 (8 bytes skipped): 
    0x43b8 vertex array stride bytes: 40
    0x43bc vertex component declaration list, # entries: 5
    0x43c0 vertex component declaration list offset: 0x43c4
    
    0x00004390|              02 00 00 40   15 00 00 00  02 00 00 00      ...@........
    0x000043a0| 00 00 00 00  00 00 00 00   78 12 02 00  cc 2d 04 00  ........x....-..
    0x000043b0| 00 00 00 00  00 00 00 00   28 00 00 00  05 00 00 00  ........(.......
    0x000043c0| 04 00 00 00                                          ....            
    
    ----------------------------------------------------------------------
    vertex component declaration list (5 entries)
    0x000043c4 - 0x000043d7
    0x43c4 listEntry: 0x43d8
    0x43c8 listEntry: 0x440c
    0x43cc listEntry: 0x4440
    0x43d0 listEntry: 0x4474
    0x43d4 listEntry: 0x44a8
    
    0x000043c0|              14 00 00 00   44 00 00 00  74 00 00 00      ....D...t...
    0x000043d0| a4 00 00 00  d4 00 00 00                             ........        
    
    ----------------------------------------------------------------------
    component declaration
    0x000043d8 - 0x0000440b
    0x43d8 flags: 1000 0000 0000 0000 0000 0000 0000 0010
    0x43dc component type: Just CmpPos
    0x43e0 (28 bytes skipped): 
    0x43fc component data type: Just CDFloat
    0x43fd (3 bytes skipped): 
    0x4400 # of component values: 3
    0x4404 component value multiplier: 1.0
    0x4408 component position: 0
    
    0x000043d0|                            01 00 00 40  00 00 00 00          ...@....
    0x000043e0| 00 00 00 00  00 00 00 00   00 00 00 00  00 00 00 00  ................
    0x000043f0| 00 00 00 00  00 00 00 00   00 00 00 00  06 14 00 00  ................
    0x00004400| 03 00 00 00  00 00 80 3f   00 00 00 00               .......?....    



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



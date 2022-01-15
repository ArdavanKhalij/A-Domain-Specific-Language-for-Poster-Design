%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adding the libraries.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module([library(lists), io, library(pcre)]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Each property of the types
% Divide the height and width of the poster and make them integers
to_integer(WidthxHeight, Width, Height):-
    re_split(x, WidthxHeight, [W, _, H]),
    atom_number(H, Height),
    atom_number(W, Width).
% dimensions property
dimensions_command(Row, Cols) --> ['\t', dimensions, :, WidthxHeight, '\n'], {to_integer(WidthxHeight, Row, Cols)}.
% filename property
filename_command(FileName) --> ['\t', filename, :, '"',  FileName, '"', '\n'].
% content property
content_command(Content) --> ['\t', content, :, '"'|Content], ['"', '\n'], {\+member('\n', Content)}.
% image property
source_command(Source) --> ['\t', source, :, '"', Source, '"', '\n'].
% position property
% All available positions
available_position('top-edge').
available_position('bottom-edge').
available_position('left-edge').
available_position('right-edge').
available_position('top-left').
available_position('top-right').
available_position('bottom-left').
available_position('bottom-right').
position_command(Position) --> ['\t', position, :, Position, '\n'], {available_position(Position)}.
% aspect property
aspect_command(Width, Height) --> ['\t', aspect, :, WidthxHeight, '\n'], {to_integer(WidthxHeight, Width, Height)}.
% size property
size_command(Width, Height) --> ['\t', size, :, WidthxHeight, '\n'], {to_integer(WidthxHeight, Width, Height)}.
% width property
% percent width
% convert the number and percent sign to only a number
to_integer_percent(Output, Input):-
    atom_string(Input, InputString),
    split_string(InputString, "%", "", [InputStringWithoutPercent,_]),
    atom_number(InputStringWithoutPercent, Output).
width_command_percent(Width) --> ['\t', width, :, WidthWithPercent, '\n'],
                                 {to_integer_percent(Width, WidthWithPercent)}.
% absolute width
width_command_absolute(Width) --> ['\t', width, :, WidthAtom, '\n'], {atom_number(WidthAtom, Width)}.
% height property
% percent height
height_command_percent(Height) --> ['\t', height, :, HeightWithPercent, '\n'],
                                   {to_integer_percent(Height, HeightWithPercent)}.
% absolute height
height_command_absolute(Height) --> ['\t', height, :, HeightAtom, '\n'], {atom_number(HeightAtom, Height)}.
% ref property
ref_command(Ref) --> ['\t', ref, :, '"'|Ref], ['"', '\n'], {\+member('\n', Ref)}.
% adjacency property
% All available adjacency
available_adjacency(above).
available_adjacency(below).
available_adjacency(leftof).
available_adjacency(rightof).
adjacency_command(Adjacency, Ref) --> ['\t', adjacency, :, Adjacency, '"'|Ref], ['"', '\n'],
                                      {available_adjacency(Adjacency), \+member('\n', Ref)}.
% Command of all top-level properties that returns the property name and its output arguments
properties_poster([dimensions_command, Row, Cols]) --> dimensions_command(Row, Cols).
properties_poster([filename_command, FileName]) --> filename_command(FileName).
% Command of all properties that returns the property name and its output arguments
properties([content_command, Content]) --> content_command(Content).
properties([source_command, Source]) --> source_command(Source).
properties([position_command, Position]) --> position_command(Position).
properties([aspect_command, Width, Height]) --> aspect_command(Width, Height).
properties([size_command, Width, Height]) --> size_command(Width, Height).
properties([width_command_percent, Width]) --> width_command_percent(Width).
properties([width_command_absolute, Width]) --> width_command_absolute(Width).
properties([height_command_percent, Height]) --> height_command_percent(Height).
properties([height_command_absolute, Height]) --> height_command_absolute(Height).
properties([ref_command, Ref]) --> ref_command(Ref).
properties([adjacency_command, Adjacency, RefName]) --> adjacency_command(Adjacency, RefName).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% asset types
% poster top-level asset: This asset is a top-level asset and it only needs two properties and they are mandatory,
% dimensions and filename. So it can have only these two properties and it should have these two properties.
poster_command([Property1, Property2]) -->
               [poster, :, '\n'], properties_poster(Property1), properties_poster(Property2).
% figure with at least one and maximum 9 properties
figure_command([Property1]) --> [figure, :, '\n'], properties(Property1).
figure_command([Property1, Property2]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2).
figure_command([Property1, Property2, Property3]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2), properties(Property3).
figure_command([Property1, Property2, Property3, Property4]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
figure_command([Property1, Property2, Property3, Property4, Property5]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               [figure, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% image with at least one and maximum 9 properties
image_command([Property1]) --> [image, :, '\n'], properties(Property1).
image_command([Property1, Property2]) -->
               [image, :, '\n'], properties(Property1), properties(Property2).
image_command([Property1, Property2, Property3]) -->
               [image, :, '\n'], properties(Property1), properties(Property2), properties(Property3).
image_command([Property1, Property2, Property3, Property4]) -->
               [image, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
image_command([Property1, Property2, Property3, Property4, Property5]) -->
               [image, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
image_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               [image, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
image_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               [image, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
image_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               [image, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
image_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               [image, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% text with at least one and maximum 9 properties
text_command([Property1]) --> [text, :, '\n'], properties(Property1).
text_command([Property1, Property2]) -->
               [text, :, '\n'], properties(Property1), properties(Property2).
text_command([Property1, Property2, Property3]) -->
               [text, :, '\n'], properties(Property1), properties(Property2), properties(Property3).
text_command([Property1, Property2, Property3, Property4]) -->
               [text, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
text_command([Property1, Property2, Property3, Property4, Property5]) -->
               [text, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
text_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               [text, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
text_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               [text, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
text_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               [text, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
text_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               [text, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% caption with at least one and maximum 9 properties
caption_command([Property1]) --> [caption, :, '\n'], properties(Property1).
caption_command([Property1, Property2]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2).
caption_command([Property1, Property2, Property3]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2), properties(Property3).
caption_command([Property1, Property2, Property3, Property4]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
caption_command([Property1, Property2, Property3, Property4, Property5]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               [caption, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% title with at least one and maximum 9 properties
title_command([Property1]) --> [title, :, '\n'], properties(Property1).
title_command([Property1, Property2]) -->
               [title, :, '\n'], properties(Property1), properties(Property2).
title_command([Property1, Property2, Property3]) -->
               [title, :, '\n'], properties(Property1), properties(Property2), properties(Property3).
title_command([Property1, Property2, Property3, Property4]) -->
               [title, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
title_command([Property1, Property2, Property3, Property4, Property5]) -->
               [title, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
title_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               [title, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
title_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               [title, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
title_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               [title, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
title_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               [title, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% section with at least one and maximum 9 properties
section_command([Property1]) --> [section, :, '\n'], properties(Property1).
section_command([Property1, Property2]) -->
               [section, :, '\n'], properties(Property1), properties(Property2).
section_command([Property1, Property2, Property3]) -->
               [section, :, '\n'], properties(Property1), properties(Property2), properties(Property3).
section_command([Property1, Property2, Property3, Property4]) -->
               [section, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
section_command([Property1, Property2, Property3, Property4, Property5]) -->
               [section, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
section_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               [section, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
section_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               [section, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
section_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               [section, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
section_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               [section, :, '\n'], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

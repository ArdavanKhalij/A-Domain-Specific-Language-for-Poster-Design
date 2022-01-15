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
dimensions_command(Row, Cols) --> ['\n', '\t', dimensions, :, WidthxHeight], {to_integer(WidthxHeight, Row, Cols)}.
% filename property
filename_command(FileName) --> ['\n', '\t', filename, :, '"',  FileName, '"'].
% content property
content_command(Content) --> ['\n', '\t', content, :, '"'|Content], ['"'], {\+member('\n', Content)}.
% image property
source_command(Source) --> ['\n', '\t', source, :, '"', Source, '"'].
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
position_command(Position) --> ['\n', '\t', position, :, Position], {available_position(Position)}.
% aspect property
aspect_command(Width, Height) --> ['\n', '\t', aspect, :, WidthxHeight], {to_integer(WidthxHeight, Width, Height)}.
% size property
size_command(Width, Height) --> ['\n', '\t', size, :, WidthxHeight], {to_integer(WidthxHeight, Width, Height)}.
% width property
% percent width
% convert the number and percent sign to only a number
to_integer_percent(Output, Input):-
    atom_string(Input, InputString),
    split_string(InputString, "%", "", [InputStringWithoutPercent,_]),
    atom_number(InputStringWithoutPercent, Output).
width_command_percent(Width) --> ['\n', '\t', width, :, WidthWithPercent],
                                 {to_integer_percent(Width, WidthWithPercent)}.
% absolute width
width_command_absolute(Width) --> ['\n', '\t', width, :, WidthAtom], {atom_number(WidthAtom, Width)}.
% height property
% percent height
height_command_percent(Height) --> ['\n', '\t', height, :, HeightWithPercent],
                                   {to_integer_percent(Height, HeightWithPercent)}.
% absolute height
height_command_absolute(Height) --> ['\n', '\t', height, :, HeightAtom], {atom_number(HeightAtom, Height)}.
% ref property
ref_command(Ref) --> ['\n', '\t', ref, :, '"'|Ref], ['"'], {\+member('\n', Ref)}.
% adjacency property
% All available adjacency
available_adjacency(above).
available_adjacency(below).
available_adjacency(leftof).
available_adjacency(rightof).
adjacency_command(Adjacency, Ref) --> ['\n', '\t', adjacency, :, Adjacency, '"'|Ref], ['"'],
                                      {available_adjacency(Adjacency), \+member('\n', Ref)}.
% Command of all top-level properties that returns the property name and its output arguments
properties_poster([dimensions_command, Row, Cols]) --> dimensions_command(Row, Cols).
properties_poster([filename_command, FileName]) --> filename_command(FileName).
% Command of all properties that returns the property name and its output arguments
properties([content_command, Content]) --> content_command(Contents), {atomic_list_concat(Contents,' ',Content)}.
properties([source_command, Source]) --> source_command(Source).
properties([position_command, Position]) --> position_command(Position).
properties([aspect_command, Width, Height]) --> aspect_command(Width, Height).
properties([size_command, Width, Height]) --> size_command(Width, Height).
properties([width_command_percent, Width]) --> width_command_percent(Width).
properties([width_command_absolute, Width]) --> width_command_absolute(Width).
properties([height_command_percent, Height]) --> height_command_percent(Height).
properties([height_command_absolute, Height]) --> height_command_absolute(Height).
properties([ref_command, Ref]) --> ref_command(Refs), {atomic_list_concat(Refs,' ',Ref)}.
properties([adjacency_command, Adjacency, Ref]) --> adjacency_command(Adjacency, RefName),
    {atomic_list_concat(RefName,' ',Ref)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% asset types
% poster top-level asset: This asset is a top-level asset and it only needs two properties and they are mandatory,
% dimensions and filename. So it can have only these two properties and it should have these two properties.
poster_command([Property1, Property2]) -->
               [poster, :], properties_poster(Property1), properties_poster(Property2).
% figure with at least one and maximum 9 properties
figure_command([Property1]) --> ['\n', figure, :], properties(Property1).
figure_command([Property1, Property2]) -->
               ['\n', figure, :], properties(Property1), properties(Property2).
figure_command([Property1, Property2, Property3]) -->
               ['\n', figure, :], properties(Property1), properties(Property2), properties(Property3).
figure_command([Property1, Property2, Property3, Property4]) -->
               ['\n', figure, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
figure_command([Property1, Property2, Property3, Property4, Property5]) -->
               ['\n', figure, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               ['\n', figure, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               ['\n', figure, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               ['\n', figure, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
figure_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               ['\n', figure, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% image with at least one and maximum 9 properties
image_command([Property1]) --> ['\n', image, :], properties(Property1).
image_command([Property1, Property2]) -->
               ['\n', image, :], properties(Property1), properties(Property2).
image_command([Property1, Property2, Property3]) -->
               ['\n', image, :], properties(Property1), properties(Property2), properties(Property3).
image_command([Property1, Property2, Property3, Property4]) -->
               ['\n', image, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
image_command([Property1, Property2, Property3, Property4, Property5]) -->
               ['\n', image, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
image_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               ['\n', image, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
image_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               ['\n', image, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
image_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               ['\n', image, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
image_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               ['\n', image, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% text with at least one and maximum 9 properties
text_command([Property1]) --> ['\n', text, :], properties(Property1).
text_command([Property1, Property2]) -->
               ['\n', text, :], properties(Property1), properties(Property2).
text_command([Property1, Property2, Property3]) -->
               ['\n', text, :], properties(Property1), properties(Property2), properties(Property3).
text_command([Property1, Property2, Property3, Property4]) -->
               ['\n', text, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
text_command([Property1, Property2, Property3, Property4, Property5]) -->
               ['\n', text, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
text_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               ['\n', text, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
text_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               ['\n', text, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
text_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               ['\n', text, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
text_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               ['\n', text, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% caption with at least one and maximum 9 properties
caption_command([Property1]) --> ['\n', caption, :], properties(Property1).
caption_command([Property1, Property2]) -->
               ['\n', caption, :], properties(Property1), properties(Property2).
caption_command([Property1, Property2, Property3]) -->
               ['\n', caption, :], properties(Property1), properties(Property2), properties(Property3).
caption_command([Property1, Property2, Property3, Property4]) -->
               ['\n', caption, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
caption_command([Property1, Property2, Property3, Property4, Property5]) -->
               ['\n', caption, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               ['\n', caption, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               ['\n', caption, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               ['\n', caption, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
caption_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               ['\n', caption, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% title with at least one and maximum 9 properties
title_command([Property1]) --> ['\n', title, :], properties(Property1).
title_command([Property1, Property2]) -->
               ['\n', title, :], properties(Property1), properties(Property2).
title_command([Property1, Property2, Property3]) -->
               ['\n', title, :], properties(Property1), properties(Property2), properties(Property3).
title_command([Property1, Property2, Property3, Property4]) -->
               ['\n', title, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
title_command([Property1, Property2, Property3, Property4, Property5]) -->
               ['\n', title, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
title_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               ['\n', title, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
title_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               ['\n', title, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
title_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               ['\n', title, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
title_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               ['\n', title, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
% section with at least one and maximum 9 properties
section_command([Property1]) --> ['\n', section, :], properties(Property1).
section_command([Property1, Property2]) -->
               ['\n', section, :], properties(Property1), properties(Property2).
section_command([Property1, Property2, Property3]) -->
               ['\n', section, :], properties(Property1), properties(Property2), properties(Property3).
section_command([Property1, Property2, Property3, Property4]) -->
               ['\n', section, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4).
section_command([Property1, Property2, Property3, Property4, Property5]) -->
               ['\n', section, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5).
section_command([Property1, Property2, Property3, Property4, Property5, Property6]) -->
               ['\n', section, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6).
section_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7]) -->
               ['\n', section, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7).
section_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8]) -->
               ['\n', section, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8).
section_command([Property1, Property2, Property3, Property4, Property5, Property6, Property7, Property8, Property9]) -->
               ['\n', section, :], properties(Property1), properties(Property2), properties(Property3),
               properties(Property4), properties(Property5), properties(Property6), properties(Property7),
               properties(Property8), properties(Property9).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% combine the assets: poster should be always at first
asset1(poster_command, Result) --> poster_command(Result).
assets(figure_command, Result) --> figure_command(Result).
assets(image_command, Result) --> image_command(Result).
assets(text_command, Result) --> text_command(Result).
assets(caption_command, Result) --> caption_command(Result).
assets(title_command, Result) --> title_command(Result).
assets(section_command, Result) --> section_command(Result).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The compelete parser
dcg_parser([Asset1, Result1]) --> asset1(Asset1, Result1).
dcg_parser([Asset1, Result1, Asset2, Result2]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9, Asset10, Result10]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9), assets(Asset10, Result10).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9, Asset10, Result10, Asset11, Result11]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9), assets(Asset10, Result10), assets(Asset11, Result11).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9, Asset10, Result10, Asset11, Result11, Asset12, Result12]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9), assets(Asset10, Result10), assets(Asset11, Result11), assets(Asset12, Result12).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9, Asset10, Result10, Asset11, Result11, Asset12, Result12, Asset13,
    Result13]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9), assets(Asset10, Result10), assets(Asset11, Result11), assets(Asset12, Result12),
    assets(Asset13, Result13).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9, Asset10, Result10, Asset11, Result11, Asset12, Result12, Asset13,
    Result13, Asset14, Result14]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9), assets(Asset10, Result10), assets(Asset11, Result11), assets(Asset12, Result12),
    assets(Asset13, Result13), assets(Asset14, Result14).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9, Asset10, Result10, Asset11, Result11, Asset12, Result12, Asset13,
    Result13, Asset14, Result14, Asset15, Result15]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9), assets(Asset10, Result10), assets(Asset11, Result11), assets(Asset12, Result12),
    assets(Asset13, Result13), assets(Asset14, Result14), assets(Asset15, Result15).
dcg_parser([Asset1, Result1, Asset2, Result2, Asset3, Result3, Asset4, Result4, Asset5, Result5, Asset6, Result6, Asset7,
    Result7, Asset8, Result8, Asset9, Result9, Asset10, Result10, Asset11, Result11, Asset12, Result12, Asset13,
    Result13, Asset14, Result14, Asset15, Result15, Asset16, Result16]) -->
    asset1(Asset1, Result1), assets(Asset2, Result2), assets(Asset3, Result3), assets(Asset4, Result4),
    assets(Asset5, Result5), assets(Asset6, Result6), assets(Asset7, Result7), assets(Asset8, Result8),
    assets(Asset9, Result9), assets(Asset10, Result10), assets(Asset11, Result11), assets(Asset12, Result12),
    assets(Asset13, Result13), assets(Asset14, Result14), assets(Asset15, Result15), assets(Asset16, Result16).
run_dcg(File, Atoms, X):-
    input(File, Atoms),
    phrase(dcg_parser(X), Atoms).

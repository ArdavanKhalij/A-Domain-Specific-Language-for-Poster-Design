%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adding the libraries.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module([library(lists), io, library(pcre), library(clpfd)]).
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dimensions property
dimensions_command(Row, Cols) --> ['\n', '\t', dimensions, :, WidthxHeight], {to_integer(WidthxHeight, Row, Cols)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename property
filename_command(FileName) --> ['\n', '\t', filename, :, '"',  FileName, '"'].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% content property
content_command(Content) --> ['\n', '\t', content, :, '"'|Content], ['"'], {\+member('\n', Content)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% image property
source_command(Source) --> ['\n', '\t', source, :, '"', Source, '"'].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
position_command(Position) --> ['\n', '\t', position, :, Position], {available_position(Position)}.
% aspect property
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
aspect_command(Width, Height) --> ['\n', '\t', aspect, :, WidthxHeight], {to_integer(WidthxHeight, Width, Height)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% size property
size_command(Width, Height) --> ['\n', '\t', size, :, WidthxHeight], {to_integer(WidthxHeight, Width, Height)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% width property
% percent width
% convert the number and percent sign to only a number
to_integer_percent(Output, Input):-
    atom_string(Input, InputString),
    split_string(InputString, "%", "", [InputStringWithoutPercent,_]),
    atom_number(InputStringWithoutPercent, Output).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
width_command_percent(Width) --> ['\n', '\t', width, :, WidthWithPercent],
                                 {to_integer_percent(Width, WidthWithPercent)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% absolute width
width_command_absolute(Width) --> ['\n', '\t', width, :, WidthAtom], {atom_number(WidthAtom, Width)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% height property
% percent height
height_command_percent(Height) --> ['\n', '\t', height, :, HeightWithPercent],
                                   {to_integer_percent(Height, HeightWithPercent)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% absolute height
height_command_absolute(Height) --> ['\n', '\t', height, :, HeightAtom], {atom_number(HeightAtom, Height)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ref property
ref_command(Ref) --> ['\n', '\t', ref, :, '"'|Ref], ['"'], {\+member('\n', Ref)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adjacency property
% All available adjacency
available_adjacency(above).
available_adjacency(below).
available_adjacency(leftof).
available_adjacency(rightof).
adjacency_command(Adjacency, Ref) --> ['\n', '\t', adjacency, :, Adjacency, '"'|Ref], ['"'],
                                      {available_adjacency(Adjacency), \+member('\n', Ref)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Command of all top-level properties that returns the property name and its output arguments
properties_poster([dimensions_command, Row, Cols]) --> dimensions_command(Row, Cols).
properties_poster([filename_command, FileName]) --> filename_command(FileName).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
poster_command([Property1, Property2]) --> [poster, :], properties_poster(Property1), properties_poster(Property2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG command for properties to prevent the duplication in code for using in assets. We dont need this for poster
% because we only have one possible way to have poster asset and its with to properties that are special for poster.
properties_for_using_in_assets([Property1]) --> properties(Property1).
properties_for_using_in_assets([Property1|Tail]) --> properties(Property1), properties_for_using_in_assets(Tail).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% figure with at least one and maximum 9 properties
figure_command([Property1]) --> ['\n', figure, :], properties(Property1).
figure_command(Properties) --> ['\n', figure, :], properties_for_using_in_assets(Properties).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% image with at least one and maximum 9 properties
image_command([Property1]) --> ['\n', image, :], properties(Property1).
image_command(Properties) --> ['\n', image, :], properties_for_using_in_assets(Properties).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% text with at least one and maximum 9 properties
text_command([Property1]) --> ['\n', text, :], properties(Property1).
text_command(Properties) --> ['\n', text, :], properties_for_using_in_assets(Properties).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caption with at least one and maximum 9 properties
caption_command([Property1]) --> ['\n', caption, :], properties(Property1).
caption_command(Properties) --> ['\n', caption, :], properties_for_using_in_assets(Properties).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% title with at least one and maximum 9 properties
title_command([Property1]) --> ['\n', title, :], properties(Property1).
title_command(Properties) --> ['\n', title, :], properties_for_using_in_assets(Properties).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% section with at least one and maximum 9 properties
section_command([Property1]) --> ['\n', section, :], properties(Property1).
section_command(Properties) --> ['\n', section, :], properties_for_using_in_assets(Properties).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% combine the assets: poster should be always at first
assets([poster_command, Result]) --> poster_command(Result).
assets([figure_command, Result]) --> figure_command(Result).
assets([image_command, Result]) --> image_command(Result).
assets([text_command, Result]) --> text_command(Result).
assets([caption_command, Result]) --> caption_command(Result).
assets([title_command, Result]) --> title_command(Result).
assets([section_command, Result]) --> section_command(Result).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The compelete parser
dcg_parser([Result1]) --> assets(Result1).
dcg_parser([Result1|Tail]) -->
    assets(Result1),
    dcg_parser(Tail).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info is the compelete parsed information
run_dcg(File, Output, Info):-
    input(File, Output),
    phrase(dcg_parser(Info), Output).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generat the IDs.
generate_ID(Assets, ID):-
    length(Assets, X),
    findall(N, between(1, X, N), ID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generat the boxes.
boxes_generator([Head], ID, [Result]):-
    append(ID, Head, Result).
boxes_generator([Head|Tail], [IDH|IDT], [H|T]):-
    append([IDH], Head, Result),
    H = Result,
    boxes_generator(Tail, IDT, T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_poster_from_boxes([_|Boxes], Boxes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dimensionofposter(Row, Col, [[_, _, [[_, Row, Col]|_]]|_]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checking if the first asset is the poster and otherwise return false
check_if_first_asset_is_poster([[AssetName|_]|_]):-
    AssetName = poster_command.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract the Type from the Box. The type of the box can only be one of: text, image, header.
box_type([_, Type|_], text):-
    Type = text_command.
box_type([_, Type|_], text):-
    Type = caption_command.
box_type([_, Type|_], image):-
    Type = image_command.
box_type([_, Type|_], image):-
    Type = figure_command.
box_type([_, Type|_], header):-
    Type = title_command.
box_type([_, Type|_], header):-
    Type = section_command.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract the Id from the Box. Each box must have an associated unique Id, like a number, letter, or string.
% ID of each box is its index in the list of the boxes
box_id([ID, _|_], Id):-
    ID #= Id.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on position.
available_position_int('top-edge', 1).
available_position_int('bottom-edge', 2).
available_position_int('left-edge', 3).
available_position_int('right-edge', 4).
available_position_int('top-left', 5).
available_position_int('top-right', 6).
available_position_int('bottom-left', 7).
available_position_int('bottom-right', 8).
position_constraint(Row, Col, 'top-edge', R0, R1, C0, C1):-
    R1 #= 10 * Row div 100,
    C0 #= 1,
    C1 #= 10 * Col div 100,
    R0 #= 1.
position_constraint(Row, Col, 'bottom-edge', R0, R1, C0, C1):-
    R1 #= Row,
    R0 #= Row - 10 * Row div 100,
    C0 #= 1,
    C1 #= 10 * Col div 100.
position_constraint(Row, Col, 'left-edge', R0, R1, C0, C1):-
    R0 #= 1,
    R1 #= 10 * Row div 100,
    C0 #= 1,
    C1 #= 10 * Col div 100.
position_constraint(Row, Col, 'right-edge', R0, R1, C0, C1):-
    R0 #= 1,
    R1 #= 10 * Row div 100,
    C1 #= Col,
    C0 #= Col - 10 * Col div 100.
position_constraint(Row, Col, 'top-left', R0, R1, C0, C1):-
    R0 #= 1,
    R1 #= 10 * Row div 100,
    C0 #= 1,
    C1 #= 10 * Col div 100.
position_constraint(Row, Col, 'top-right', R0, R1, C0, C1):-
    R0 #= 1,
    R1 #= 10 * Row div 100,
    C1 #= Col,
    C0 #= Col - 10 * Col div 100.
position_constraint(Row, Col, 'bottom-left', R0, R1, C0, C1):-
    R1 #= Row,
    R0 #= Row - 10 * Row div 100,
    C0 #= 1,
    C1 #= 10 * Col div 100.
position_constraint(Row, Col, 'bottom-right', R0, R1, C0, C1):-
    R1 #= Row,
    R0 #= Row - 10 * Row div 100,
    C1 #= Col,
    C0 #= Col - 10 * Col div 100.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on width in percent.
width_percent_constraint(Col, PercentOfWidth, 1, CC1, C0, C1):-
    C1 #=Col * PercentOfWidth div 100 #<== Col * PercentOfWidth div 100 #=< Col,
    C0 #= 1.
width_percent_constraint(Col, PercentOfWidth, CC0, Col, C0, C1):-
    C1 #= Col,
    C0 #= Col+1 - Col * PercentOfWidth div 100 #<== Col+1 - Col * PercentOfWidth div 100 #> 0.
width_percent_constraint(Col, PercentOfWidth, CC0, CC1, C0, C1):-
    CC0 #\= 1,
    CC1 #\= Col,
    C0 #= CC0,
    C1 #= CC1 + Col * PercentOfWidth div 100 - 1,
    C1 #=< Col.
width_percent_constraint(Col, PercentOfWidth, CC0, CC1, C0, C1):-
    CC0 #\= 1,
    CC1 #\= Col,
    MIDDLE #= Col div 2,
    Width #= Col * PercentOfWidth div 100,
    Widthdiv2 #= Width div 2,
    C0 #= MIDDLE-Widthdiv2+1,
    C1 #= MIDDLE+Widthdiv2-1,
    C0 #> 0,
    C1 #=< Col.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on height in percent.
height_percent_constraint(Row, PercentOfHeight, 1, RR1, R0, R1):-
    R0 #= 1,
    R1 #= Row * PercentOfHeight div 100 #<== Row * PercentOfHeight div 100 #=< Row.
height_percent_constraint(Row, PercentOfHeight, RR0, Row, R0, R1):-
    R1 #= Row,
    R0 #= Row+1 - Row * PercentOfHeight div 100 #<== Row+1 - Row * PercentOfHeight div 100 #> 0.
height_percent_constraint(Row, PercentOfHeight, RR0, Row, R0, R1):-
    RR0 #\= 1,
    RR1 #\= Col,
    R0 #= RR0,
    R1 #= RR0 + Row * PercentOfHeight div 100 - 1,
    R1 #=< Row.
height_percent_constraint(Row, PercentOfHeight, RR0, RR1, R0, R1):-
    RR0 #\= 1,
    RR1 #\= Col,
    MIDDLE #= Row div 2,
    Height #= Row * PercentOfHeight div 100,
    Heightdiv2 #= Height div 2,
    R0 #= MIDDLE-Heightdiv2+1,
    R1 #= MIDDLE+Heightdiv2-1,
    R0 #> 0,
    R1 #=< Row.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on height.
height_constraint(Row, Height, 1, RR1, R0, R1):-
    R0 #= 1,
    R1 #= Height #<== Height #=< Row.
height_constraint(Row, Height, RR0, Row, R0, R1):-
    R1 #= Row,
    R0 #= Row+1 - Height #<== Row+1 - Height #> 0.
height_constraint(Row, Height, RR0, RR1, R0, R1):-
    RR0 #\= 1,
    RR1 #\= Col,
    R0 #= RR0,
    R1 #= RR0 + Height - 1,
    R1 #=< Row.
height_constraint(Row, Height, RR0, RR1, R0, R1):-
    RR0 #\= 1,
    RR1 #\= Col,
    MIDDLE #= Row div 2,
    Heightdiv2 #= Height div 2,
    R0 #= MIDDLE-Heightdiv2+1,
    R1 #= MIDDLE+Heightdiv2-1,
    R0 #> 0,
    R1 #=< Row.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on width.
width_constraint(Col, Width, 1, CC1, C0, C1):-
    C0 #= 1,
    C1 #= Width #<== Width #=< Col.
width_constraint(Col, Width, CC0, Col, C0, C1):-
   C1 #= Col,
   C0 #= Col+1 - Width #<== Col+1 - Width #> 0.
width_constraint(Col, Width, CC0, CC1, C0, C1):-
    CC0 #\= 1,
    CC1 #\= Col,
    C0 #= CC0,
    C1 #= CC0 + Width - 1,
    C1 #=< Col.
width_constraint(Col, Width, CC0, CC1, C0, C1):-
    CC0 #\= 1,
    CC1 #\= Col,
    MIDDLE #= Col div 2,
    Heightdiv2 #= Width div 2,
    C0 #= MIDDLE-Heightdiv2+1,
    C1 #= MIDDLE+Heightdiv2-1,
    C0 #> 0,
    C1 #=< Col.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on size.
size_constraint(Row, Col, Width, Height, RR0, RR1, CC0, CC1, R0, R1, C0, C1):-
    width_constraint(Col, Width, CC0, CC1, C0, C1),
    height_constraint(Row, Height, RR0, RR1, R0, R1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on aspect. we have one of the height or width. we can use any of them.
aspect_constraint_width(Size, Row, Col, Width, Height, RR0, CC0, R0, R1, C0, C1):-
    R0 #= RR0,
    C0 #= CC0,
    C1 #= Size + C0 #<== Size + C0 #=< Col,
    R1 #= Size div Width * Height + RR0 #<== Size div Width * Height + RR0 #=< Row.
aspect_constraint_height(Size, Row, Col, Width, Height, RR0, CC0, R0, R1, C0, C1):-
    R0 #= RR0,
    C0 #= CC0,
    R1 #= Size + R0 #<== Size + R0 #=< Row,
    C1 #= Size * Width div Height + CC0 #<== Size * Width div Height + CC0 #=< Col.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate the size of boxes based on the data we recieved from users.
gettingsizes(_, _, [], _, _, _, _, []).
gettingsizes(Row, Col, [[Name, _]|Properties], R0, R1, C0, C1, [H|T]):-
    Name = content_command,
    H = sizeofbox(R0, R1, C0, C1),
    gettingsizes(Row, Col, Properties, R0, R1, C0, C1, T).
gettingsizes(Row, Col, [[Name, _]|Properties], R0, R1, C0, C1, [H|T]):-
    Name = source_command,
    H = sizeofbox(R0, R1, C0, C1),
    gettingsizes(Row, Col, Properties, R0, R1, C0, C1, T).
gettingsizes(Row, Col, [[Name, Value]|Properties], _, _, _, _, [H|T]):-
    Name = position_command,
    position_constraint(Row, Col, Value, RX0, RX1, CX0, CX1),
    H = sizeofbox(RX0, RX1, CX0, CX1),
    gettingsizes(Row, Col, Properties, RX0, RX1, CX0, CX1, T).
gettingsizes(Row, Col, [[Name, Value1, Value2]|Properties], RR0, RR1, CC0, CC1, [H|T]):-
    Name = size_command,
    size_constraint(Row, Col, Value1, Value2, RR0, RR1, CC0, CC1, R0, R1, C0, C1),
    H = sizeofbox(R0, R1, C0, C1),
    gettingsizes(Row, Col, Properties, R0, R1, C0, C1, T).
gettingsizes(Row, Col, [[Name, Value]|Properties], RR0, RR1, CC0, CC1, [H|T]):-
    Name = width_command_percent,
    width_percent_constraint(Col, Value, CC0, CC1, C0, C1),
    H = sizeofbox(RR0, RR1, C0, C1),
    gettingsizes(Row, Col, Properties, RR0, RR1, C0, C1, T).
gettingsizes(Row, Col, [[Name, Value]|Properties], RR0, RR1, CC0, CC1, [H|T]):-
    Name = height_command_percent,
    height_percent_constraint(Row, Value, RR0, RR1, R0, R1),
    H = sizeofbox(R0, R1, CC0, CC1),
    gettingsizes(Row, Col, Properties, R0, R1, CC0, CC1, T).
gettingsizes(Row, Col, [[Name, Value]|Properties], RR0, RR1, CC0, CC1, [H|T]):-
    Name = height_command_absolute,
    height_constraint(Row, Value, RR0, RR1, R0, R1),
    H = sizeofbox(R0, R1, CC0, CC1),
    gettingsizes(Row, Col, Properties, R0, R1, CC0, CC1, T).
gettingsizes(Row, Col, [[Name, Value]|Properties], RR0, RR1, CC0, CC1, [H|T]):-
    Name = width_command_absolute,
    width_constraint(Col, Value, CC0, CC1, C0, C1),
    H = sizeofbox(RR0, RR1, C0, C1),
    gettingsizes(Row, Col, Properties, RR0, RR1, C0, C1, T).
gettingsizes(Row, Col, [[Name, Value]|Properties], RR0, RR1, CC0, CC1, [H|T]):-
    Name = width_command_absolute,
    width_constraint(Col, Value, CC0, CC1, C0, C1),
    H = sizeofbox(RR0, RR1, C0, C1),
    gettingsizes(Row, Col, Properties, RR0, RR1, C0, C1, T).
gettingsizes(Row, Col, [[Name, Value1, Value2]|Properties], RR0, RR1, CC0, CC1, [H|T]):-
    Name = aspect_command,
    Size #= CC1-CC0
    aspect_constraint_width(Size, Row, Col, Value1, Value2, RR0, CC0, R0, R1, C0, C1)
    H = sizeofbox(R0, R1, C0, C1),
    gettingsizes(Row, Col, Properties, R0, R1, C0, C1, T).
% Ref and adj is not available yet.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(File, Output, Info, Size):-
    input(File, Output),
    phrase(dcg_parser(Info), Output),
    generate_ID(Info, ID),
    boxes_generator(Info, ID, Boxes),
    dimensionofposter(Row, Col, Boxes),
    delete_poster_from_boxes(Boxes, Boxes2),
    gettingsizes(Row, Col, [[position_command, 'top-edge'],[width_command_percent, 100],[content_command, 'Volcanoes']], 1, 1, 1, 1, Size).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%getfromref(ref, Boxes, )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limited the R0, R1, C0 and C1 base on adjacency.
%adjacency_constraint(Row, Col, Adjacency, Ref_box, Rf0, Rf1, Cf0, Cf1, R0, R1, C0, C1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%find_position([_, _, List], yes):-
%    append(List, NewList),
%    member(position_command, NewList).
%find_position([_], no).
%
%find_position([_, _, List], yes):-
%    append(List, NewList),
%    member(width_command_percent, NewList).
%find_position([_], no).
%
%box_col([_, _, List], C0, C1):-
%    append(List, C0),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

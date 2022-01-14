%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adding the libraries.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module([library(lists), io, library(pcre)]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Divide the height and width of the poster and make them integers
to_integer(WidthxHeight, Width, Height):-
    re_split(x, WidthxHeight, [W, _, H]),
    atom_number(H, Height),
    atom_number(W, Width).
% each property of the types
% dimensions property
dimensions_command(Width, Height) --> ['\t', dimensions, :, WidthxHeight, '\n'], {to_integer(WidthxHeight, Width, Height)}.
% filename property
filename_command(FileName) --> ['\t', filename, :, '"',  FileName, '"', '\n'].
% content property
content_command(Content) --> ['\t', content, :, '"'|Content], ['"', '\n'].
% image property
source_command(Source) --> ['\t', source, :, '"', Source, '"', '\n'].
% position property
% All available positions
available_position(top-edge).
available_position(bottom-edge).
available_position(left-edge).
available_position(right-edge).
available_position(top-left).
available_position(top-right).
available_position(bottom-left).
available_position(bottom-right).
position_command(Position) --> ['\t', position, :, Position, '\n'], {available_position(Position)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
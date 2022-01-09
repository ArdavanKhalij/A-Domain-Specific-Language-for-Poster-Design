%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IO Library Code for Final Project: Domain Specific Language for Poster Design

:- module(io, [input/2, output/4]).

:- use_module([library(clpfd), library(readutil), library(pcre), library(apply)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert internal box representations to standard form.

box(Box, box( id = Id, 
              type = Type, 
              row = (R0, R1),
              col = (C0, C1), 
              content = Content )) :-
  user:box_id(Box, Id),
  user:box_type(Box, Type),
  user:box_row(Box, R0, R1),
  user:box_col(Box, C0, C1),
  user:box_content(Box, Content).

boxes([], []).
boxes([Box|Boxes], [Wrapped|Wrappeds]) :-
  box(Box, Wrapped),
  boxes(Boxes, Wrappeds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG to generate and output HTML and CSS based on the given Layout

n --> ['\n'].

html(dims(Nx, Ny), Boxes) --> 
  ['<!DOCTYPE html>'], n,
  ['<html>'], n,
  ['<head>'], n,
  ['<style>'], n,
  grid_style(Nx, Ny),
  shared_properties,
  box_styles(Boxes),
  ['</style'], n,
  ['</head>'], n,
  ['<body>'], n,
  ['<div class="grid">'], n,
  box_els(Boxes),
  ['</div>'], n,
  ['</body>'], n,
  ['</html>'], n.

grid_style(Nx, Ny) --> 
  ['.grid {'], n,
  {Width #= Nx * 20},
  {Height #= Ny * 20},
  ['width: ', Width, 'px;'], n,
  ['height: ', Height, 'px;'], n,
  ['display: grid;'], n,
  ['place-items: center;'], n,
  ['grid-template-columns: repeat(', Nx, ', 20px);'], n,
  ['grid-template-rows: repeat(', Ny, ', 20px);'], n,
  ['border: 2px solid grey;'], n,
  ['border-radius: 5px;'], n,
  ['background: whitesmoke;'], n, 
  ['overflow: hidden;'], n,
  ['}'], n.

shared_properties -->
  ['.box {'], n,
  ['border: 2px solid grey;'], n,
  ['border-radius: 5px;'], n,
  ['overflow: hidden;'], n,
  ['background: white;'], n,
  ['width: 100%;'], n,
  ['height: 100%;'], n,
  ['}'], n.

box_styles([]) --> [].
box_styles([Box|Boxes]) --> box_style(Box), box_styles(Boxes).

box_style(box(id = Id,
              type = _Type, 
              row = (R0, R1),
              col = (C0, C1),
              content = _Content)) -->
  {atom_concat('#box', Id, BoxId)},
  [BoxId, ' {'], n,
  ['grid-column:', C0, '/', C1, ';'], n,
  ['grid-row:', R0, '/', R1, ';'], n,
  ['}'], n.

box_els([]) --> [].
box_els([Box|Boxes]) --> box_el(Box), box_els(Boxes).

box_el(box(id = Id,
           type = Type,
           row = (_, _),
           col = (_, _),
           content = Content)) -->
  {atom_concat(box, Id, BoxId)},
  ['<div class="box" id="', BoxId, '">'], n,
  box_inner(Type, Content),
  ['</div>'], n.
box_inner(text, Content) -->
  ['<p>', Content, '</p>'], n.
box_inner(image, Content) -->
  ['<img src="', Content,'" alt="', Content, '"></img>'], n.
box_inner(header, Content) -->
  ['<h1>', Content, '</h1>'], n.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG to parse .poster files into list of atoms

string_atoms([], []).
string_atoms([" ", " "|Strings], ['\t'|Atoms]) :-
  string_atoms(Strings, Atoms).
string_atoms([" "|Strings], Atoms) :-
  string_atoms(Strings, Atoms).
string_atoms([String|Strings], [Atom|Atoms]) :-
  atom_string(Atom, String),
  string_atoms(Strings, Atoms).

strip_empty([], []).
strip_empty([""|Strings], Stripped) :-
  strip_empty(Strings, Stripped).
strip_empty([String|Strings], [String|Stripped]) :- 
  String \= "",
  strip_empty(Strings, Stripped).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API

% input(+File, -Atoms)
%   Parse the contents of the .poster file to a list of Atoms
input(File, Atoms) :-
  read_file_to_string(File, String, []),
  re_split('[:\"\n\s\t]', String, Strings),
  strip_empty(Strings, Stripped),
  string_atoms(Stripped, Atoms),
  !.
  
% output(+Layout, +Filename, +Width, +Height)
%   Generate an HTML+CSS file with Filename from the provided Layout
output(Layout, Filename, Width, Height) :-
  open(Filename, write, Fd),
  user:layout_boxes(Layout, Boxes),
  boxes(Boxes, Wrapped),
  phrase(html(dims(Width, Height), Wrapped), Atoms),
  atomic_list_concat(Atoms, String),
  write(Fd, String),
  close(Fd),
  !.
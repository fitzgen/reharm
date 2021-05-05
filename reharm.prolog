:- use_module(library(clpfd)).

is_note(X) :-
    0 #=< X,
    X #< 12.

%% Names %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

note_name(0, "A").
note_name(1, "B♭").
note_name(2, "B").
note_name(3, "C").
note_name(4, "D♭").
note_name(5, "D").
note_name(6, "E♭").
note_name(7, "E").
note_name(8, "F").
note_name(9, "G♭").
note_name(10, "G").
note_name(11, "A♭").

note_names(Notes, Names) :-
    maplist(note_name, Notes, Names).

:- discontiguous chord_name/2.

chord_name_suffix(Chord, Suffix, Name) :-
    [Root|_] = Chord,
    note_name(Root, RootName),
    string_concat(RootName, Suffix, Name).

chord_details(Chord, Details) :-
    chord_name(Chord, Name),
    string_concat(Name, " = [", Details0),
    append_note_names(Chord, false, Details0, Details1),
    string_concat(Details1, "]", Details).

append_note_names([], _, Str, Str).
append_note_names([Note|Notes], true, Str, OutStr) :-
    note_name(Note, Name),
    string_concat(Str, ", ", Str1),
    string_concat(Str1, Name, Str2),
    append_note_names(Notes, true, Str2, OutStr).
append_note_names([Note|Notes], false, Str, OutStr) :-
    note_name(Note, Name),
    string_concat(Str, Name, Str1),
    append_note_names(Notes, true, Str1, OutStr).

%% Scales and Modes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

major_scale([One, Two, Three, Four, Five, Six, Seven]) :-
    is_note(One),
    is_note(Two),
    is_note(Three),
    is_note(Four),
    is_note(Five),
    is_note(Six),
    is_note(Seven),
    Two #= (One + 2) mod 12,
    Three #= (Two + 2) mod 12,
    Four #= (Three + 1) mod 12,
    Five #= (Four + 2) mod 12,
    Six #= (Five + 2) mod 12,
    Seven #= (Six + 2) mod 12.

natural_minor_scale([One, Two, Three, Four, Five, Six, Seven]) :-
    is_note(One),
    is_note(Two),
    is_note(Three),
    is_note(Four),
    is_note(Five),
    is_note(Six),
    is_note(Seven),
    Two #= (One + 2) mod 12,
    Three #= (Two + 1) mod 12,
    Four #= (Three + 2) mod 12,
    Five #= (Four + 2) mod 12,
    Six #= (Five + 1) mod 12,
    Seven #= (Six + 2) mod 12.

harmonic_minor_scale([One, Two, Three, Four, Five, Six, Seven]) :-
    is_note(One),
    is_note(Two),
    is_note(Three),
    is_note(Four),
    is_note(Five),
    is_note(Six),
    is_note(Seven),
    Two #= (One + 2) mod 12,
    Three #= (Two + 1) mod 12,
    Four #= (Three + 2) mod 12,
    Five #= (Four + 2) mod 12,
    Six #= (Five + 1) mod 12,
    Seven #= (Six + 3) mod 12.

diatonic(_, []).
diatonic(Mode, [Note|Rest]) :-
    is_note(Note),
    member(Note, Mode),
    diatonic(Mode, Rest).

%% Intervals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

major_third(Note, Third) :-
    is_note(Note),
    is_note(Third),
    Third #= (Note + 4) mod 12.

minor_third(Note, Third) :-
    is_note(Note),
    is_note(Third),
    Third #= (Note + 3) mod 12.

diminished_third(Note, Third) :-
    is_note(Note),
    is_note(Third),
    Third #= (Note + 2) mod 12.

%% Triads %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

major_triad_chord([Root, Third, Fifth]) :-
    major_third(Root, Third),
    minor_third(Third, Fifth).

chord_name(Chord, Name) :-
    major_triad_chord(Chord),
    chord_name_suffix(Chord, " major", Name).

minor_triad_chord([Root, Third, Fifth]) :-
    minor_third(Root, Third),
    major_third(Third, Fifth).

chord_name(Chord, Name) :-
    minor_triad_chord(Chord),
    chord_name_suffix(Chord, " minor", Name).

diminished_triad_chord([Root, Third, Fifth]) :-
    minor_third(Root, Third),
    minor_third(Third, Fifth).

chord_name(Chord, Name) :-
    diminished_triad_chord(Chord),
    chord_name_suffix(Chord, " diminished", Name).

augmented_triad_chord([Root, Third, Fifth]) :-
    major_third(Root, Third),
    major_third(Third, Fifth).

chord_name(Chord, Name) :-
    augmented_triad_chord(Chord),
    chord_name_suffix(Chord, " augmented", Name).

%% Seventh Chords %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

major_seventh_chord([Root, Third, Fifth, Seventh]) :-
    major_triad_chord([Root, Third, Fifth]),
    major_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    major_seventh_chord(Chord),
    chord_name_suffix(Chord, " major 7", Name).

minor_seventh_chord([Root, Third, Fifth, Seventh]) :-
    minor_triad_chord([Root, Third, Fifth]),
    minor_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    minor_seventh_chord(Chord),
    chord_name_suffix(Chord, " minor 7", Name).

dominant_seventh_chord([Root, Third, Fifth, Seventh]) :-
    major_triad_chord([Root, Third, Fifth]),
    minor_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    dominant_seventh_chord(Chord),
    chord_name_suffix(Chord, "7", Name).

minor_major_seventh_chord([Root, Third, Fifth, Seventh]) :-
    minor_third(Root, Third),
    major_third(Third, Fifth),
    major_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    minor_major_seventh_chord(Chord),
    chord_name_suffix(Chord, " minor major 7", Name).

diminished_seventh_chord([Root, Third, Fifth, Seventh]) :-
    diminished_triad_chord([Root, Third, Fifth]),
    minor_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    diminished_seventh_chord(Chord),
    chord_name_suffix(Chord, " diminished 7", Name).

half_diminished_seventh_chord([Root, Third, Fifth, Seventh]) :-
    diminished_triad_chord([Root, Third, Fifth]),
    major_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    half_diminished_seventh_chord(Chord),
    chord_name_suffix(Chord, " half-diminished 7", Name).

augmented_seventh_chord([Root, Third, Fifth, Seventh]) :-
    augmented_triad_chord([Root, Third, Fifth]),
    diminished_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    augmented_seventh_chord(Chord),
    chord_name_suffix(Chord, " augmented 7", Name).

augmented_major_seventh_chord([Root, Third, Fifth, Seventh]) :-
    augmented_triad_chord([Root, Third, Fifth]),
    minor_third(Fifth, Seventh).

chord_name(Chord, Name) :-
    augmented_seventh_chord(Chord),
    chord_name_suffix(Chord, " major augmented 7", Name).

%% TODO: sus4 chords

seventh_chord(Chord) :-
    (
        major_seventh_chord(Chord);
        minor_seventh_chord(Chord);
        dominant_seventh_chord(Chord);
        minor_major_seventh_chord(Chord);
        diminished_seventh_chord(Chord);
        half_diminished_seventh_chord(Chord);
        augmented_seventh_chord(Chord);
        augmented_major_seventh_chord(Chord)
    ).

%% Guide Tones and Available Tensions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

guide_tones([_Root, Third, _Fifth, Seventh], [Third, Seventh]).

chord_tone_plus_two(Note, Tension) :-
    is_note(Note),
    is_note(Tension),
    Tension #= (Note + 2) mod 12.

available_tensions(Chord, Tensions) :-
    (
        major_seventh_chord(Chord)
    ;   minor_seventh_chord(Chord)
    ;   minor_major_seventh_chord(Chord)
    ;   diminished_seventh_chord(Chord)
    ;   half_diminished_seventh_chord(Chord)
    ),
    maplist(chord_tone_plus_two, Chord, Tensions).

available_tensions(Chord, Tensions) :-
    augmented_major_seventh_chord(Chord),
    [Root, Third, _, _] = Chord,
    Tensions = [
        (Root + 2) mod 12,
        (Third + 2) mod 12
    ].

available_tensions(Chord, Tensions) :-
    dominant_seventh_chord(Chord),
    [Root, Third, Fifth, _] = Chord,
    Tensions = [
        (Root + 1) mod 12,
        (Root + 2) mod 12,
        (Root + 3) mod 12,
        (Third + 2) mod 12,
        (Fifth + 1) mod 12,
        (Fifth + 2) mod 12
    ].

available_tensions(Chord, Tensions) :-
    augmented_seventh_chord(Chord),
    [Root, Third, Fifth, _] = Chord,
    Tensions = [
        (Root + 1) mod 12,
        (Root + 2) mod 12,
        (Root + 3) mod 12,
        (Third + 2) mod 12,
        (Fifth + 1) mod 12
    ].

%% Reharm %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_members([], _).
all_members([X|Xs], Ys) :-
    member(X, Ys),
    all_members(Xs, Ys).

reharm(MelodyNotes, Chord) :-
    seventh_chord(Chord),
    guide_tones(Chord, Tones),
    available_tensions(Chord, Tensions),
    append(Tones, Tensions, TonesAndTensions),
    all_members(MelodyNotes, TonesAndTensions).

reharm_with_weak_harmonies(MelodyNotes, Chord) :-
    seventh_chord(Chord),
    available_tensions(Chord, Tensions),
    append(Chord, Tensions, TonesAndTensions),
    all_members(MelodyNotes, TonesAndTensions).

reharm_all(MelodyNotes, Chords) :-
    findall(
        Chord,
        reharm(MelodyNotes, Chord),
        ChordsList
    ),
    list_to_set(ChordsList, Chords).

reharm_all_with_weak_harmonies(MelodyNotes, Chords) :-
    findall(
        Chord,
        reharm_with_weak_harmonies(MelodyNotes, Chord),
        ChordsList
    ),
    list_to_set(ChordsList, Chords).

#!/usr/bin/perl -w
# $Id$

my $version =
  [ '$Id$ (Release 1.00)',
    'The server is written and Copyright 2000 by Rocco Caputo. All rights ' .
    'are reserved. This program is free code; it may be distributed and/or ' .
    'modified under the same terms as Perl itself. The author may be ' .
    'reached by e-mail at <troc+xws@netrus.net>. Notes, including a to-do ' .
    'list, are in the comments at the end of the program. This server is ' .
    'based on the POE framework, available from your friendly neighborhood ' .
    'CPAN mirror. Thanks for reading!'
  ];

use strict;
use lib '../poe'; # For developer testing against the latest POE
use Cwd qw(abs_path);
use POE qw( Component::Server::TCP Wheel::ReadWrite Filter::Line Driver::SysRW
          );

# The server will shut down or restart when a signal is caught.  This
# "global" flag contains the type of shutdown requested: restart,
# shutdown, or signal shutdown (default).
my $shutdown_type = 'signal';

# Parse apart $0 and use Cwd to find the absolute path to the program.
# The assumption is that the crossword puzzle files share that
# directory or aren't very far from it.  This will be replaced with a
# directive in the xws.conf file later.  Sorry about the
# DOS/Unix-centric hack here.

my ($xws_path, $xws_program) = ($0 =~ /^(.*)[\\\/](.*?)$/);
$xws_path = abs_path $xws_path;
$xws_program = './' . $xws_program;
chdir($xws_path) or die "Cannot change directory to $xws_path: $!";

###############################################################################
# Deal with Across Lite crossword puzzle files.  This will be tidied
# and commented at some point, but the other mechanisms (which are
# more visible to the end user) take precedence.

package Puzzle::Crossword::AcrossLite;

# Puzzle structure offsets.

sub PUZ_FILE       () {  0 }
sub PUZ_SIZE_X     () {  1 }
sub PUZ_SIZE_Y     () {  2 }
sub PUZ_TITLE      () {  3 }
sub PUZ_AUTHOR     () {  4 }
sub PUZ_COPYRIGHT  () {  5 }
sub PUZ_GRID       () {  6 }
sub PUZ_CLUES      () {  7 }
sub PUZ_NOTES      () {  8 }
sub PUZ_REMAINING  () {  9 }
sub PUZ_PLACE_GRID () { 10 }

# Clue structure offsets.

sub CLUE_TEXT    () { 0 }
sub CLUE_WORD    () { 1 }
sub CLUE_DONE    () { 2 }
sub CLUE_PATTERN () { 3 }
sub CLUE_POS_Y   () { 4 }
sub CLUE_POS_X   () { 5 }
sub CLUE_INC_Y   () { 6 }
sub CLUE_INC_X   () { 7 }
sub CLUE_WRONG   () { 8 }
sub CLUE_CROSSES () { 9 }

# Crossword status values.

sub XWS_SUCCESS       () { 0 }
sub XWS_NO_ANSWER     () { 1 }
sub XWS_NO_CLUE       () { 2 }
sub XWS_ALREADY_DONE  () { 3 }
sub XWS_WRONG_SIZE    () { 4 }
sub XWS_WRONG_ANSWER  () { 5 }
sub XWS_NO_PERMISSION () { 6 }
sub XWS_NO_PUZZLE     () { 7 }

# Import to other packages.

sub import {
  my $caller_package = caller();
  no strict 'refs';
  *{$caller_package . '::XWS_SUCCESS'}       = \&XWS_SUCCESS;
  *{$caller_package . '::XWS_NO_ANSWER'}     = \&XWS_NO_ANSWER;
  *{$caller_package . '::XWS_NO_CLUE'}       = \&XWS_NO_CLUE;
  *{$caller_package . '::XWS_ALREADY_DONE'}  = \&XWS_ALREADY_DONE;
  *{$caller_package . '::XWS_WRONG_SIZE'}    = \&XWS_WRONG_SIZE;
  *{$caller_package . '::XWS_WRONG_ANSWER'}  = \&XWS_WRONG_ANSWER;
  *{$caller_package . '::XWS_NO_PERMISSION'} = \&XWS_NO_PERMISSION;
  *{$caller_package . '::XWS_NO_PUZZLE'}     = \&XWS_NO_PUZZLE;
}

# Create a new crossword puzzle.

sub new {
  my $type = shift;
  my $self = bless [ ], $type;

  $self;
}

# Internal function to compare clues, sorting by across or down, then
# my number.

sub _clue_compare {
  ( (substr($a,-1) cmp substr($b,-1)) ||
    (substr($a, 0, length($a)-1) <=> substr($b, 0, length($b)-1) )
  );
}

# Load a crossword puzzle.  Builds the puzzle and clue structures, and
# returns a true/false value indicating success.

sub load {
  my ($self, $filename) = @_;

  if (open PUZZLE, $filename) {
    binmode(PUZZLE);

    my $read_size;

    # Get the puzzle size.

    seek PUZZLE, 0x2C, 0 or do { close PUZZLE; return 0; };
    my $x_extent = ord getc PUZZLE;
    my $y_extent = ord getc PUZZLE;
    my $puzzle_size = $x_extent * $y_extent;

    # Get the answers grid.

    seek PUZZLE, 0x34, 0 or do { close PUZZLE; return 0; };
    my @answers_grid;
    for (my $read_counter = 0; $read_counter < $y_extent; $read_counter++) {
      read PUZZLE, my $line = '', $x_extent or do { close PUZZLE; return 0; };
      push @answers_grid, [ split '', $line ];
    }

    # Make the blank and place grids.

    my (@blank_grid, @place_grid);
    for (my $read_counter = 0; $read_counter < $y_extent; $read_counter++) {
      read PUZZLE, my $line = '', $x_extent or do { close PUZZLE; return 0; };

      $line =~ s/\./\|\#\#/g;
      $line =~ s/\-/\|\_\_/g;
      1 while ($line =~ s/\#\#\|\#\#/\#\#\#\#\#/g);

      push @blank_grid, $line;

      push @place_grid, [];
      for (my $place_index = 0; $place_index < $x_extent; $place_index++) {
        push @{$place_grid[-1]}, [];
      }
    }

    # Get the puzzle headers.

    local $/ = chr(0);
    my $title = <PUZZLE>;
    chomp $title;
    $title =~ s/\s+/ /g;
    $title =~ s/^\s+//g;
    $title =~ s/\s+$//g;
    $title = '(untitled)' unless (defined($title) and length($title));

    my $author = <PUZZLE>;
    chomp $author;
    $author =~ s/\s+/ /g;
    $author =~ s/^\s+//g;
    $author =~ s/\s+$//g;
    $author = '(anonymous)' unless (defined($author) and length($author));

    my $copy = <PUZZLE>;
    chomp $copy;
    $copy =~ s/\s+/ /g;
    $copy =~ s/^\s+//g;
    $copy =~ s/\s+$//g;
    $copy =~ s/\xA9/(c)/g;
    $copy = '(unknown)' unless (defined($copy) and length($copy));

    # Get clues.

    my @clues;
    while (<PUZZLE>) {
      chomp;
      s/\s+/ /g;
      s/^\s+//g;
      s/\s+$//g;
      push(@clues, $_) if (defined($_) and length);
    }

    close PUZZLE;

    # Store the easy things.

    $self->[PUZ_FILE]      = $filename;
    $self->[PUZ_SIZE_X]    = $x_extent;
    $self->[PUZ_SIZE_Y]    = $y_extent;
    $self->[PUZ_TITLE]     = $title;
    $self->[PUZ_AUTHOR]    = $author;
    $self->[PUZ_COPYRIGHT] = $copy;

    # This is a little harder.  Combine the clues and the answer grid
    # to get a clue structure.  See the CLUE_* constants for fields.

    my $remaining_count = 0;
    my $clue_number = 0;
    my %clues;

    for (my $y = 0; $y < $y_extent; $y++) {
      for (my $x = 0; $x < $x_extent; $x++) {

        # Skip blocked cells.
        next if $answers_grid[$y][$x] eq '.';

        # Helpers to calculate clue numbers.
        my $this_clue_number = 0;
        my $next_answer = sub {
          unless ($this_clue_number) {
            $this_clue_number = ++$clue_number;
            my $pretty_grid_number = sprintf("%2d_", $this_clue_number % 100);
            $pretty_grid_number =~ tr[ ][|];
            substr($blank_grid[$y], $x * 3, 3) = $pretty_grid_number;
          }
          $this_clue_number;
        };

        # If this is the left column, or if the cell immediately to
        # the left is filled, then this starts an across clue.
        if (!$x or $answers_grid[$y][$x-1] eq '.') {

          # Build the answer and a list of coordinates for it.
          my ($answer, @answer_places);
          for ( my $word_x = $x;
                $word_x < $x_extent and $answers_grid[$y][$word_x] ne '.';
                $word_x++
              ) {
            push @answer_places, [ $y, $word_x ];
            $answer .= $answers_grid[$y][$word_x];
          }

          # Answers only count if they're more than one letter.
          if (length($answer) > 1) {
            my @crossed_answers;

            # Calculate the clue ID.
            my $this_clue_id = &$next_answer . 'a';

            # Cross-reference this clue with others that interset it.
            for (my $place=0; $place<@answer_places; $place++) {
              my ($ap_y, $ap_x) = @{$answer_places[$place]};

              # For every known crossing word...
              foreach my $other_place (@{$place_grid[$ap_y][$ap_x]}) {

                # Record the other word's cross place with this clue.
                push @crossed_answers, $other_place->[0];

                # Record this word's cross place with that clue.
                push( @{$clues{$other_place->[0]}->[CLUE_CROSSES]},
                      &$next_answer
                    );
              }

              # Record the crossing point in the place grid, which is
              # now virtually obsolete after the puzzle is loaded.
              push(@{$place_grid[$ap_y][$ap_x]}, [$this_clue_id, $place]);
            }

            # Record the clue.
            $clues{$this_clue_id} =
              [ shift @clues,           # Clue text
                $answer,                # Answer text
                '',                     # Solver nickname
                ('-' x length($answer)), # Solved pattern
                $y, $x, 0, 1,           # Start coordinates and direction
                { },                    # List of failed tries
                \@crossed_answers,      # List of crossing clues, by place
              ];

            # Count this clue.
            $remaining_count++;
          }
        }

        # If this is the top row, or if the cell immediately above
        # this one is filled, then this starts a down clue.
        if (!$y or $answers_grid[$y-1][$x] eq '.') {

          # Build the answer and a list of coordinates for it.
          my ($answer, @answer_places);
          for ( my $word_y = $y;
                $word_y < $y_extent and $answers_grid[$word_y][$x] ne '.';
                $word_y++
              ) {
            push @answer_places, [ $word_y, $x ];
            $answer .= $answers_grid[$word_y][$x];
          }

          # Answers only count if they're more than one letter.
          if (length($answer) > 1) {
            my @crossed_answers;

            # Calculate the clue ID.
            my $this_clue_id = &$next_answer . 'd';

            # Cross-reference this clue with others that interset it.
            for (my $place=0; $place<@answer_places; $place++) {
              my ($ap_y, $ap_x) = @{$answer_places[$place]};

              # For every known crossing word...
              foreach my $other_place (@{$place_grid[$ap_y][$ap_x]}) {

                # Record the other word's cross place with this clue.
                push @crossed_answers, $other_place->[0];

                # Record this word's cross place with that clue.
                push( @{$clues{$other_place->[0]}->[CLUE_CROSSES]},
                      &$next_answer
                    );
              }

              # Record the crossing point in the place grid, which is
              # now virtually obsolete after the puzzle is loaded.
              push(@{$place_grid[$ap_y][$ap_x]}, [$this_clue_id, $place]);
            }

            # Record the clue.
            $clues{$this_clue_id} =
              [ shift @clues,           # Clue text
                $answer,                # Answer text
                '',                     # Solver nickname
                ('-' x length($answer)), # Solved pattern
                $y, $x, 1, 0,           # Start coordinates and direction
                { },                    # List of failed tries
                \@crossed_answers,      # List of crossing clues, by place
              ];

            # Count this clue.
            $remaining_count++;
          }
        }
      }
    }

    # Save the clues and other difficult things.

    $self->[PUZ_CLUES]      = \%clues;
    $self->[PUZ_REMAINING]  = $remaining_count;
    $self->[PUZ_GRID]       = \@blank_grid;
    $self->[PUZ_PLACE_GRID] = \@place_grid;

    # Anything left in the clues list is notes.  Notes may include
    # spoilers, so they're obscured (locked away) until the puzzle has
    # been solved.

    $self->[PUZ_NOTES] = [ grep !/^\s*$/, @clues ];

    return 1;
  }

  return 0;
}

# Try to solve a clue.  Returns 0 or a positive error code, plus
# return-dependent fields.
sub answer {
  my ($self, $whom, $clue_id, $try) = @_;
  $clue_id = lc $clue_id;

  return XWS_NO_PUZZLE unless defined $self->[PUZ_FILE];
  return XWS_NO_ANSWER unless length($try);
  return XWS_NO_CLUE   unless exists $self->[PUZ_CLUES]->{$clue_id};

  my $clue = $self->[PUZ_CLUES]->{$clue_id};

  return(XWS_ALREADY_DONE, $clue->[CLUE_DONE]) if length $clue->[CLUE_DONE];
  return(XWS_WRONG_SIZE, length($clue->[CLUE_WORD]))
    if length($clue->[CLUE_WORD]) != length($try);

  my (@got, @add, @del);
  $try = uc $try;

  # Test the clue against the word we have.

  if ($clue->[CLUE_WORD] eq $try) {

    # Mark the clue as done, and record it for the return value.

    $clue->[CLUE_DONE] = $whom;
    $clue->[CLUE_PATTERN] = $try;
    push @got, [ $clue_id, $try, --$self->[PUZ_REMAINING] ];
    push @del, $clue_id;

    # Plug each letter in the clue into the grid.  While we are doing
    # this, check to see if we accidentally fill in a word we cross.

    my $y  = $clue->[CLUE_POS_Y];
    my $x  = $clue->[CLUE_POS_X];
    my $dy = $clue->[CLUE_INC_Y];
    my $dx = $clue->[CLUE_INC_X];

    my $letter_index = 0;
    foreach (split '', $try) {

      # Fill the grid cell for the letter.

      my $cell = substr($self->[PUZ_GRID]->[$y], ($x * 3), 2) . $_;
      $cell =~ tr[_][ ];
      substr($self->[PUZ_GRID]->[$y], ($x * 3), 3) = $cell;

      # Remove surrounding lines, if possible.

      if ($x + 1 < $self->[PUZ_SIZE_X]) {
        my $bar = substr($self->[PUZ_GRID]->[$y], ($x * 3) + 3, 1);
        substr($self->[PUZ_GRID]->[$y], ($x * 3) + 3, 1) = ' ' if $bar eq '|';
      }
      if ( ($x > 0) and
           (substr($self->[PUZ_GRID]->[$y], ($x * 3) - 1, 2) eq '#|')
         ) {
        substr($self->[PUZ_GRID]->[$y], ($x * 3), 1) = ' ';
      }

      # Fill this letter into each word that shares it.

      foreach my $crossing_answer (@{$self->[PUZ_PLACE_GRID]->[$y][$x]}) {
        my ($cross_id, $cross_index) = @$crossing_answer;

        # Skip the clue if it's ours.
        next if ($cross_id eq $clue_id);

        my $fill_clue = $self->[PUZ_CLUES]->{$cross_id};

        # Don't bother if the crossed clue is also finished.
        next if (length($fill_clue->[CLUE_DONE]));

        substr($fill_clue->[CLUE_PATTERN], $cross_index, 1) = $_;

        # Check for an incidental fill.

        if ($fill_clue->[CLUE_PATTERN] !~ /-/) {
          $fill_clue->[CLUE_DONE] = $whom;
          push  @got, [ $cross_id, $fill_clue->[CLUE_PATTERN],
                        --$self->[PUZ_REMAINING]
                      ];
          push @del, $cross_id;
        }
        else {
          push @add, $cross_id;
        }
      }

      $y += $dy;
      $x += $dx;
      $letter_index++;
    }
  }

  # Otherwise the clue and try don't match.  Save the bad try in case
  # someone is curious.
  else {
    return(XWS_WRONG_ANSWER, $clue->[CLUE_WRONG]->{$try}++);
  }

  return( XWS_SUCCESS,
          \@got,
          [ sort _clue_compare @add ],
          [ sort _clue_compare @del ],
        );
}

# Return a list of failed attempts at this clue.
sub get_tries {
  my ($self, $clue_id) = @_;

  return XWS_NO_PUZZLE unless defined $self->[PUZ_FILE];

  $clue_id = lc $clue_id;
  unless (exists $self->[PUZ_CLUES]->{$clue_id}) {
    return XWS_NO_CLUE;
  }

  return( XWS_SUCCESS,
          sort keys %{$self->[PUZ_CLUES]->{$clue_id}->[CLUE_WRONG]}
        );
}

# Returns a picture of the puzzle grid.
sub get_grid {
  my $self = shift;

  return XWS_NO_PUZZLE unless defined $self->[PUZ_FILE];

  my @grid = ' ' . ('-' x ($self->[PUZ_SIZE_X] * 3 - 1));

  foreach (@{$self->[PUZ_GRID]}) {
    push @grid, $_ . '|';
  }

  push @grid, $grid[0];
  return (XWS_SUCCESS, @grid);
}

# Returns a list of clues by direction, either just the unsolved ones
# or all of them.
sub get_clues {
  my ($self, $direction, $all) = @_;

  return XWS_NO_PUZZLE unless defined $self->[PUZ_FILE];

  my @pending;
  foreach ( sort _clue_compare keys %{$self->[PUZ_CLUES]} ) {
    next unless /$direction$/i;
    my $clue = $self->[PUZ_CLUES]->{$_};
    next unless (!length($clue->[CLUE_DONE]) or $all);
    push( @pending,
          [ $_,
            $clue->[CLUE_TEXT],
            $clue->[CLUE_PATTERN],
            $clue->[CLUE_DONE],
            $clue->[CLUE_CROSSES],
          ]
        );
  }

  return (XWS_SUCCESS, @pending);
}

# Retrieve a clue by its ID.  If the ID doesn't specify across or
# down, then try both.
sub get_clue_by_id {
  my ($self, $clue_id) = @_;

  return XWS_NO_PUZZLE unless defined $self->[PUZ_FILE];

  my @clues;
  $clue_id = lc $clue_id;

  foreach my $suffix ('', 'a', 'd') {
    if (exists $self->[PUZ_CLUES]->{$clue_id . $suffix}) {
      my $clue = $self->[PUZ_CLUES]->{$clue_id . $suffix};
      push( @clues,
            [ $clue_id . $suffix,
              $clue->[CLUE_TEXT],
              $clue->[CLUE_PATTERN],
              $clue->[CLUE_DONE],
              $clue->[CLUE_CROSSES],
            ]
          );
    }
  }

  if (@clues) {
    return(XWS_SUCCESS, @clues);
  }

  return XWS_NO_CLUE;
}

# Return some information about the puzzle.
sub get_headers {
  my $self = shift;

  return XWS_NO_PUZZLE unless defined $self->[PUZ_FILE];

  return
    ( XWS_SUCCESS,
      [ File      => $self->[PUZ_FILE]      ],
      [ Title     => $self->[PUZ_TITLE]     ],
      [ Author    => $self->[PUZ_AUTHOR]    ],
      [ Copyright => $self->[PUZ_COPYRIGHT] ],
    );
}

# Notes can include solutions; that's bad.  Make fetching notes a
# separate function.

sub get_notes {
  my $self = shift;
  my @notes;

  return XWS_NO_PUZZLE unless defined $self->[PUZ_FILE];

  if ($self->[PUZ_REMAINING]) {
    return XWS_NO_PERMISSION;
  }

  return(XWS_SUCCESS, @notes);
}

sub sort_clue_ids {
  my $self = shift;
  sort _clue_compare @_;
}

###############################################################################
# A "Chat" is a group of actors sharing the same virtual space.
# Messages publicly presented are multiplexed to everyone within
# the ranges of sight or sound.

use strict;

package POE::Component::Chat;

use vars qw($VERSION);
$VERSION = 1.00;

# Explicit use to import the parameter constants.
use POE::Session;

# Actor structure.
sub ACTOR_SESSION () { 0 }
sub ACTOR_NICK    () { 1 }

# Chat object sturcture.
sub CHAT_ACTORS   () { 'chat_actors' }

# Chat events.  These will be fired at actors' sessions when standard
# chat things occur.  Other events should be provided directly to the
# Chat::plex, and they will faithfully be passed along to actors'
# sessions.
sub CHEV_VISIBLE () { 'chat_visible' }
sub CHEV_AUDIBLE () { 'chat_audible' }
sub CHEV_EVENT   () { 'chat_event'   }

# Chat message types.  These are the ones we emit.
sub CHMT_NIL      () { 'nil'      }
sub CHMT_LOGIN    () { 'login'    }
sub CHMT_LOGOUT   () { 'logout'   }
sub CHMT_NICK     () { 'nickname' }
sub CHMT_WHO      () { 'who'      }

# Import to other packages.  This tends to be bad.

sub import {
  my $caller_package = caller();
  no strict 'refs';
  *{$caller_package . '::CHEV_VISIBLE'} = \&CHEV_VISIBLE;
  *{$caller_package . '::CHEV_AUDIBLE'} = \&CHEV_AUDIBLE;
  *{$caller_package . '::CHEV_EVENT'}   = \&CHEV_EVENT;
  *{$caller_package . '::CHMT_NIL'}     = \&CHMT_NIL;
  *{$caller_package . '::CHMT_LOGIN'}   = \&CHMT_LOGIN;
  *{$caller_package . '::CHMT_LOGOUT'}  = \&CHMT_LOGOUT;
  *{$caller_package . '::CHMT_NICK'}    = \&CHMT_NICK;
  *{$caller_package . '::CHMT_WHO'}     = \&CHMT_WHO;
}

# Create a new chat multiplexer.

sub new {
  my ($type, $alias) = @_;

  POE::Session->create
    ( inline_states =>
      { _start   => \&chat_start,
        log_in   => \&chat_login,
        log_out  => \&chat_logout,
        plex     => \&chat_plex,
        say      => \&chat_say,
        emote    => \&chat_emote,
        nick     => \&chat_nick,
        who      => \&chat_who,
        get_nick => \&chat_get_nick,
      },
      args => [ $alias ],
    );

  undef;
}

# The chat server has been created.  Set it up.

sub chat_start {
  $_[HEAP]->{CHAT_ACTORS} = { };
  $_[KERNEL]->alias_set($_[ARG0]);
}

# Internal multiplexer helper.  This is not a POE state.

sub _internal_plex {
  my ($kernel, $heap, $type, $subtype, $actor, @things) = @_;
  foreach my $actor_rec (values(%{$heap->{CHAT_ACTORS}})) {
    $kernel->call( $actor_rec->[ACTOR_SESSION], $type,
                   $subtype, $actor, @things
                 );
  }
}

# Log an actor's session into the chatter.

sub chat_login {
  my ($kernel, $heap, $sender, $nick) = @_[KERNEL, HEAP, SENDER, ARG0];

  my $actor_rec = $heap->{CHAT_ACTORS}->{$sender} = [ ];
  $actor_rec->[ACTOR_SESSION] = $sender;
  $actor_rec->[ACTOR_NICK]    = $nick;

  &_internal_plex($kernel, $heap, CHEV_EVENT, CHMT_LOGIN, $sender);
}

# Log an actor's session out of the chatter.

sub chat_logout {
  my ($kernel, $heap, $sender) = @_[KERNEL, HEAP, SENDER];

  &_internal_plex($kernel, $heap, CHEV_EVENT, CHMT_LOGOUT, $sender);

  # Must delete the chat actor after the plex, so that the actor's
  # nick can be looked up in the plex.
  delete $heap->{CHAT_ACTORS}->{$sender};
}

# Helper to say something.

sub chat_say {
  my ($kernel, $heap, $sender) = @_[KERNEL, HEAP, SENDER];
  &_internal_plex( $kernel, $heap, CHEV_AUDIBLE, CHMT_NIL,
                   $sender, @_[ARG0..$#_]
                 );
}

# Helper to do something.

sub chat_emote {
  my ($kernel, $heap, $sender) = @_[KERNEL, HEAP, SENDER];
  &_internal_plex( $kernel, $heap, CHEV_VISIBLE, CHMT_NIL,
                   $sender,  @_[ARG0..$#_]
                 );
}

# Helper to change a nickname.

sub chat_nick {
  my ($kernel, $heap, $sender, $new_nick) = @_[KERNEL, HEAP, SENDER, ARG0];
  my $old_nick = $heap->{CHAT_ACTORS}->{$sender}->[ACTOR_NICK];
  $heap->{CHAT_ACTORS}->{$sender}->[ACTOR_NICK] = $new_nick;
  &_internal_plex($kernel, $heap, CHEV_EVENT, CHMT_NICK, $sender, $old_nick);
}

# External plex entry point.

sub chat_plex {
  my ($kernel, $heap, $sender, $event, @args) =
    @_[KERNEL, HEAP, SENDER, ARG0..$#_];
  &_internal_plex($kernel, $heap, CHEV_EVENT, $event, $sender, @args);
}

# This one is meant to be called.  It returns a list of session
# references, sorted by nickname.

sub chat_who {
  my ($kernel, $heap, $sender) = @_[KERNEL, HEAP, SENDER];
  my @names = ( sort
                map { $_->[ACTOR_NICK] }
                values %{$heap->{CHAT_ACTORS}}
              );
  \@names;
}

# This one's meant to be called.  It returns a nick for a given
# session.
sub chat_get_nick {
  $_[HEAP]->{CHAT_ACTORS}->{$_[ARG0]}->[ACTOR_NICK];
}

###############################################################################
# This is the crossword server CLI session.  It drives the crossword
# puzzle and/or chatter.

package UI::Crossword::CLI;

use Socket;
use Cwd qw(cwd);
use Digest::MD5 qw(md5_base64);
BEGIN { Puzzle::Crossword::AcrossLite->import(); }
BEGIN { POE::Component::Chat->import(); }

# Explicit use to import the parameter constants.
use POE::Session;

sub CM_BAD_INPUT    () { 'bad input'    }
sub CM_ADMIN_OK     () { 'admin ok'     }
sub CM_ADMIN_BAD    () { 'admin not ok' }
sub CM_LOAD_BAD     () { 'load bad'     }
sub CM_LOAD_OK      () { 'load ok'      }
sub CM_DIR_EMPTY    () { 'dir empty'    }
sub CM_DIR_BAD      () { 'dir bad'      }
sub CM_DIR_FILES    () { 'dir files'    }
sub CM_PWD          () { 'pwd'          }
sub CM_VERSION      () { 'version'      }
sub CM_UPTIME       () { 'uptime'       }
sub CM_NO_PUZZLE    () { 'no puzzle'    }
sub CM_NO_CLUE      () { 'no clue'      }
sub CM_NOT_GOD      () { 'not god'      }
sub CM_NO_NOTES     () { 'no notes'     }
sub CM_TRIES        () { 'tries'        }
sub CM_ACROSS       () { 'across'       }
sub CM_DOWN         () { 'down'         }
sub CM_BOTH         () { 'both'         }
sub CM_CLUE         () { 'clue'         }
sub CM_CLUE_SHORT   () { 'clue short'   }
sub CM_WRONG_SIZE   () { 'wrong size'   }
sub CM_ANSWER_DONE  () { 'answer done'  }
sub CM_GOT_WORD     () { 'got one'      }
sub CM_GOT_NONE     () { 'got none'     }
sub CM_HELP         () { 'help'         }
sub CM_ADMIN_HELP   () { 'admin help'   }
sub CM_SHOW_GRID    () { 'show grid'    }
sub CM_ABOUT        () { 'about'        }
sub CM_NOTES        () { 'notes'        }
sub CM_WHAT         () { 'what'         }
sub CM_TOUCH        () { 'touch'        }
sub CM_TOUCHED      () { 'touched'      }
sub CM_QUIT         () { 'quit'         }
sub CM_RESTART      () { 'restart'      }
sub CM_SHUTDOWN     () { 'shutdown'     }
sub CM_SIGNAL       () { 'signal'       }
sub CM_ANSI         () { 'ansi'         }

sub new {
  my ( $type, $crossword, $password,
       $socket, $remote_address, $remote_port
     ) = @_;

  create POE::Session
    ( inline_states =>

      # From POE::Kernel:
      { _start    => \&session_start,
        _stop     => \&session_stop,
        signals   => \&session_signals,

        # From the chat server:
        &CHEV_VISIBLE => \&session_sees,
        &CHEV_AUDIBLE => \&session_hears,
        &CHEV_EVENT   => \&session_event,

        # From the ReadWrite wheel:
        got_error => \&session_error,
        got_flush => \&session_flushed,
        got_input => \&session_input,
      },

      args => [ $crossword, $password,
                $socket, $remote_address, $remote_port
              ],
    );

  undef;
}

#------------------------------------------------------------------------------
# The session has been created.  Set it up.  Register a signal
# handler, and begin reading and writing on the client socket.  Set
# initial connection states, and greet the client.
sub session_start {
  my ( $kernel, $heap,
       $puzzle, $password,
       $socket, $remote_address, $remote_port
     ) = @_[KERNEL, HEAP, ARG0..ARG4];

  $kernel->sig( KILL => 'signals' );
  $kernel->sig( INT  => 'signals' );
  $kernel->sig( TERM => 'signals' );
  $kernel->sig( STOP => 'signals' );

  $heap->{wheel} = new POE::Wheel::ReadWrite
    ( Handle       => $socket,
      Filter       => new POE::Filter::Line,
      Driver       => new POE::Driver::SysRW,
      ErrorState   => 'got_error',
      FlushedState => 'got_flush',
      InputState   => 'got_input',
    );

  $heap->{crossword} = $puzzle;
  $heap->{touched}   = { };
  $heap->{is_a_god}  = 0;
  $heap->{has_ansi}  = 0;
  $heap->{password}  = $password;

  # Build a default nick.
  my $initial_nick = inet_ntoa($remote_address) . ':' . $remote_port;

  # Log into the chatter.
  $kernel->call(chat => log_in => $initial_nick);
}

# This session has stopped.  Log us out of the chat multiplexor.
sub session_stop {
  $_[KERNEL]->call(chat => log_out => $_[SESSION]);
}

# Handle signals.  This sends a "goodbye" message and tells the
# "output flushed" handler to destroy the read/write wheel when output
# is done.  It also stops processing input.
sub session_signals {
  my ($kernel, $session, $heap, $signal) = @_[KERNEL, SESSION, HEAP, ARG0];

  $heap->{done} = 1;
  $heap->{wheel}->event( InputState => undef );

  if ($shutdown_type eq 'restart') {
    $kernel->yield(CHEV_EVENT, CM_RESTART, $session);
  }
  elsif ($shutdown_type eq 'shutdown') {
    $kernel->yield(CHEV_EVENT, CM_SHUTDOWN, $session);
  }
  else {
    $kernel->yield(CHEV_EVENT, CM_SIGNAL, $session, "SIG$signal");
  }
  1;
}

#------------------------------------------------------------------------------

# Helper function.  Display a number of seconds as a formatted period
# of time.
sub _elapsed {
  my ($s, $p) = @_;
  my ($t, @o);
                                        # build array of time parts
  if ($t = int($s / 604800)) { $s %= 604800; push(@o, $t . 'w'); }
  if ($t = int($s / 86400 )) { $s %= 86400;  push(@o, $t . 'd'); }
  if ($t = int($s / 3600  )) { $s %= 3600;   push(@o, $t . 'h'); }
  if ($t = int($s / 60    )) { $s %= 60;     push(@o, $t . 'm'); }
  if ($s || !scalar(@o)    ) {               push(@o, $s . 's'); }
                                        # reduce precision
  if ($p) {
    pop(@o) while (scalar(@o) > $p);
  }
                                        # return string
  join(' ', @o);
}

# Helper to format one or more clues.
sub _format_clues {
  map {
    my ($id, $text, $pattern, $whom, $crosses) = @$_;

    $whom = " ($whom)" if length $whom;
    my $cross_text = join ' ', @$crosses;
    $cross_text =~ s/^(.*),/$1 and/;

    sprintf( "%-4s: %s [%s]%s {%s}",
             $id, $text, $pattern, $whom, $cross_text
           );
  } @_;
}

#------------------------------------------------------------------------------

# Dequeue a visual message, format it, and queue it for output.
sub session_sees {
  my ($kernel, $session, $heap, $actor, $text) =
    @_[KERNEL, SESSION, HEAP, ARG1, ARG2];
  my $nick = $kernel->call(chat => get_nick => $actor);

  if ($text =~ s/<me>/<$nick>/ig) {
    $text = '[' . $text . ']';
  }
  else {
    if ($text =~ /^\W/) {
      $text = '[' . $nick . $text . ']';
    }
    else {
      $text = '[' . $nick . ' ' . $text . ']';
    }
  }

  $heap->{wheel}->put($text);
}

# Dequeue an audible message, format it, and queue it for output.
sub session_hears {
  my ($kernel, $heap, $actor, $text) = @_[KERNEL, HEAP, ARG1, ARG2];
  my $nick = $kernel->call(chat => get_nick => $actor);
  $heap->{wheel}->put("$nick: $text");
}

# Dequeue a system message, format it, and queue it for output.
sub session_event {
  my ($kernel, $session, $heap, $event, $actor, @argv) =
    @_[KERNEL, SESSION, HEAP, ARG0..$#_];
  my $nick = $kernel->call(chat => get_nick => $actor);
  my $text;

  # Chat messages.
  if ($event eq CHMT_LOGIN) {
    $text = "$nick has connected.";
  }
  elsif ($event eq CHMT_LOGOUT) {
    if ($actor ne $session) {
      $text = "$nick has disconnected.";
    }
  }
  elsif ($event eq CHMT_NICK) {
    $text = "$argv[0] is now known as $nick.";
  }
  elsif ($event eq CHMT_WHO) {
    $text = 'Clients connected: ' . join(', ', @argv);
  }

  # Our own messages.
  elsif ($event eq CM_SHUTDOWN) {
    $text = 'The server is being shut down. Goodbye!';
  }
  elsif ($event eq CM_RESTART) {
    $text = 'The server is restarting and should be back shortly. Goodbye!';
  }
  elsif ($event eq CM_SIGNAL) {
    $text = "The server caught $argv[0] and is shutting down. Goodbye!";
  }
  elsif ($event eq CM_BAD_INPUT) {
    $text = 'Rejecting input (contains control characters).';
  }
  elsif ($event eq CM_QUIT) {
    $text = 'Goodbye!';
  }
  elsif ($event eq CM_ADMIN_OK) {
    $text = 'You are now an admin. Try "admin help" for new things to do.';
  }
  elsif ($event eq CM_ADMIN_BAD) {
    $text = '"Are you a god?" "Uh... no." "Then... DIE!"';
  }
  elsif ($event eq CM_NOT_GOD) {
    $text = "I'm sorry, $nick, but I'm afraid you can't do that.";
  }
  elsif ($event eq CM_LOAD_BAD) {
    $text = "Didn't load $argv[0]. Maybe because of: $argv[1]";
  }
  elsif ($event eq CM_LOAD_OK) {
    $text = "$nick loaded a new puzzle: $argv[0].";
  }

  elsif ($event eq CM_PWD) {
    $text = "Current working directory: $argv[0]";
  }

  elsif ($event eq CM_DIR_EMPTY) {
    $text = "No files in $argv[1]";
  }

  elsif ($event eq CM_DIR_BAD) {
    $text = "Cannot list $argv[0]: $argv[1]";
  }
  elsif ($event eq CM_DIR_FILES) {
    my $directory = shift @argv;

    my $max_width = 0;
    foreach (@argv) {
      $max_width = length if length > $max_width;
    }
    $max_width += 2;
    my $columns = int(79 / $max_width);

    $text = [ scalar(@argv) . ' things in $directory:' ];

    my ($this_line, $this_columns) = ( '', 0 );
    foreach (@argv) {
      unless ($this_columns++ < $columns) {
        $this_line =~ s/\s+$//;
        push @$text, $this_line;
        $this_line = '';
        $this_columns = 1;
      }

      $this_line .= $_ . (' ' x ($max_width - length));
    }

    if (length $this_line) {
      $this_line =~ s/\s+$//;
      push @$text, $this_line;
    }
  }
  elsif ($event eq CM_VERSION) {
    $text = $version;
  }
  elsif ($event eq CM_UPTIME) {
    my ($user_time, $system_time) = (times())[0,1];
    my $wall_time = (time() - $^T) || 1;
    my $load_average = sprintf("%.4f", ($user_time+$system_time) / $wall_time);
    $text =
      ( "The crossword server is running as process $$. " .
        'It was started on ' . scalar(gmtime($^T)) . ' GMT. ' .
        'It has been active for ' . &_elapsed($wall_time, 2) . '. ' .
        sprintf( 'It has used about %.2f%% of a CPU during its lifespan.',
                 (($user_time+$system_time)/$wall_time) * 100
               )
      );
  }
  elsif ($event eq CM_NO_PUZZLE) {
    $text = "There is no puzzle loaded.";
  }
  elsif ($event eq CM_NO_NOTES) {
    $text =( "Notes may contain spoilers and are unavailable " .
             "while the puzzle is in play."
           );
  }
  elsif ($event eq CM_TRIES) {
    my ($status, @tries) = $heap->{crossword}->get_tries($argv[0]);
    if (@tries) {
      $text = "Failed attempts at $argv[0]: " . join(', ', @tries);
    }
    else {
      $text = "There have been no failed attempts at $argv[0] yet.";
    }
  }
  elsif ($event eq CM_ACROSS) {
    if (@argv) {
      $text = [ 'ACROSS:', &_format_clues(@argv) ];
    }
    else {
      $text = 'ACROSS: (none)';
    }
  }
  elsif ($event eq CM_DOWN) {
    if (@argv) {
      $text = [ 'DOWN:', &_format_clues(@argv) ];
    }
    else {
      $text = 'DOWN: (none)';
    }
  }
  elsif ($event eq CM_CLUE) {
    $text = [ &_format_clues(@argv) ];
  }
  elsif ($event eq CM_NO_CLUE) {
    $text = "There is no clue $argv[0].";
  }
  elsif ($event eq CM_WRONG_SIZE) {
    $text = ( "Sorry... $argv[0] should be $argv[2] letters, " .
              "and $argv[1] doesn't fit."
            );
  }
  elsif ($event eq CM_ANSWER_DONE) {
    $text = "Sorry... $argv[1] already answered $argv[0].";
  }
  elsif ($event eq CM_GOT_WORD) {
    my @got = @{$argv[0]};
    my @crosses = @{$argv[1]};
    $text =
      [ "$nick got $got[0]->[0], $got[0]->[1].",
        map { ( "$nick also filled in $_->[0], $_->[1]. $_->[2] " .
                (($_->[2] == 1) ? 'remains.' : 'remain.')
              )
            }
        @got[1..$#got]
      ];
    if (@crosses) {
      my $crosses = join ', ', @crosses;
      $crosses =~ s/^(.*),/$1 and/;
      $text->[0] .= " It crosses $crosses.";
    }
    $text->[0] .= ( ' ' . $got[0]->[2] .
                    (($got[0]->[2] == 1) ? ' remains.' : ' remain.')
                  );
  }
  elsif ($event eq CM_GOT_NONE) {
    $text = "Sorry... $argv[0] is not $argv[1].";
  }
  elsif ($event eq CM_HELP) {
    if (defined($argv[0]) and length($argv[0])) {
      if ($argv[0] eq 'ansi') {
        $text =
          ( '"ansi < on | off >" enables or disables ANSI features. ' .
            'These features currently only include color in the otherwise ' .
            'really busy crossword grid. Use of this feature requires a ' .
            'suitable terminal and/or client. TinyFugue may support it with ' .
            '"/set emulation ansi_attr".'
          );
      }
      elsif ($argv[0] eq 'who') {
        $text = '"who" shows the nicknames of currently connected clients.';
      }
      elsif ($argv[0] eq '?' or $argv[0] eq 'help') {
        $text =
          ( '"< ? | help > [command]" without the command shows a list of ' .
            'things that can be done. With the command, it shows more ' .
            'information about a particular thing.'
          );
      }
      elsif ($argv[0] eq 'nick') {
        $text =
          ( '"nick <nickname>" changes your nickname, which is used to ' .
            'identify yourself to others in the and while chatting.  The ' .
            'default nicknames people are given are rather unpleasant, so ' .
            'the first thing they tend to do is change it.'
          );
      }
      elsif ($argv[0] eq 'about') {
        $text =
          ( '"about" shows information about the current crossword puzzle, ' .
            'such as its name, author, and copyright.'
          );
      }
      elsif ($argv[0] eq 'notes') {
        $text =
          ( '"notes" shows additional information about the current puzzle. ' .
            'Since this tends to include answers, it is not available until ' .
            'a puzzle is solved.'
          );
      }
      elsif ($argv[0] eq 'grid') {
        $text = '"grid" shows the current state of the puzzle as a grid.';
      }
      elsif ($argv[0] eq 'down') {
        $text =
          ( '"[all] down" without the "all" shows only the unsolved ' .
            'downward clues. With "all", it shows the solved ones as well.'
          );
      }
      elsif ($argv[0] eq 'across') {
        $text =
          ( '"[all] across" without the "all" shows only the unsolved ' .
            'across clues. With "all", it shows the solved ones as well.'
          );
      }
      elsif ($argv[0] eq 'both') {
        $text =
          ( '"[all] both" without the "all" shows only the unsolved ' .
            'clues, across and downward. With "all", it shows the solved ' .
            'ones as well.'
          );
      }
      elsif ($argv[0] eq 'clue') {
        $text =
          ( '"[clue] <position>" shows the clue or clues for a particular ' .
            'spot on the grid. The "clue" word may be optional. ' .
            'For example, these are equivalent: "clue 1a" and "1a". If the ' .
            '"a" or "d" is left off, then it will display all the clues ' .
            'starting at that position. For example: "1" will show either ' .
            '1a, 1d, 1a and 1d, or none depending on which clues start there.'
          );
      }
      elsif ($argv[0] eq 'tries') {
        $text =
          ( '"tries <position>" shows you what wrong answers already have '.
            "been tried at a given position. The game doesn't record tries " .
            'that are the wrong size or that occur after the clue has been ' .
            'solved. Unlike the "clue" command, the position for "tries" ' .
            'must end with "a" or "d".'
          );
      }
      elsif ($argv[0] eq 'touch') {
        $text =
          ( '"touch" shows unsolved clues that have been partially filled ' .
            'by solved ones since the last time you used "touch".'
          );
      }
      elsif ($argv[0] eq 'uptime') {
        $text =
          '"uptime" shows runtime statistics about the crossword server.';
      }
      elsif ($argv[0] eq 'version') {
        $text =
          '"version" shows information about the crossword server software.';
      }
      elsif ($argv[0] eq 'quit') {
        $text =
          '"quit" disconnects you. You may also just disconnect your client.';
      }
      elsif ($argv[0] eq 'say') {
        $text =
          ( '"say" shares text with others. The text is formatted as if ' .
            'it was spoken. You may instead use a double quote (") instead ' .
            'of "say" to share spoken text. A trailing double quote is not ' .
            'necessary.'
          );
      }
      elsif ($argv[0] eq 'do') {
        $text =
          ( '"do" shares text with others. The text is formatted as if ' .
            'it was acted out. You may also preface this "emoted" text with ' .
            'a colon (:) or with the IRC-like command "/me". For example: ' .
            '":is sleepy." is shared as "[nickname is sleepy]". If the ' .
            'emoted text begins with punctuation, then it will be abutted ' .
            'against your nickname. For example: ":\'s sleepy" is shared as ' .
            '"[nickname\'s sleepy]". If you would like to place your ' .
            'nickname elsewhere in the emoted text, use "<me>" there. For ' .
            'example: ":This crossword has <me> stumped." is shared as ' .
            '[This crossword has <nickname> stumped.]". Your nickname ' .
            'literally will be enclosed in angle brackets when using <me>.'
          );
      }
      elsif ($argv[0] eq 'is') {
        $text =
          ( '"<position> is <word>" attempts to solve the clue at ' .
            '<position> with <word>. <position> must be a complete clue ID, ' .
            'including the trailing "a" for across or "d" for down. For ' .
            'example: "1a is stoat".'
          );
      }
      else {
        $text = ( "There is no help for '$argv[0]'. " .
                  "Try just 'help' for a list of things to do. "
                );
      }
    }

    else {
      $text =
        [ 'These are the things you may do. Things in angle brackets are ' .
          '<requried>. Things in square brackets are [optional]. Things ' .
          'separated by vertical bars are either|or options. The uppercase ' .
          'words indicate COMMANDS which have more help available (e.g., ' .
          'help is).',
          join( '; ', sort
                qw(WHO ABOUT NOTES GRID TOUCH UPTIME VERSION QUIT SAY DO),
                '< ? | HELP > [command]',
                'NICK <nickname>',
                '[all] DOWN',
                '[all] ACROSS',
                '[all] BOTH',
                '[CLUE] <position>',
                'TRIES <position>',
                '<position> IS <word>',
                'ANSI < on | off >'
              )
        ];
    }
  }

  elsif ($event eq CM_ADMIN_HELP) {
    $text =
      [ 'load <filename> - Load a new crossword puzzle.',
        'ls <path>       - Get a simple list of files in a directory.',
        'pwd             - Print the current working directory.',
        'shutdown        - Shut down... now! Also may be done with signals.',
        "restart         - exec '$xws_program'; # may not work on some OSes",
      ];
  }

  elsif ($event eq CM_SHOW_GRID) {
    $text = \@argv;
  }

  elsif ($event eq CM_ABOUT) {
    $text = [ map { "$_->[0]: $_->[1]" } @argv ];
  }

  elsif ($event eq CM_NOTES) {
    $text = map { "Note: $_" } @argv;
  }

  elsif ($event eq CM_WHAT) {
    $text = 'What?  Try "help" for things to do.';
  }

  elsif ($event eq CM_ANSI) {
    $text = 'ANSI features are ' . ($argv[0] ? 'on' : 'off') . '.';
  }

  elsif ($event eq CM_TOUCH) {
    # Doesn't set $text because it's a silent message.
    my ($op, @which) = @argv;
    if ($op eq 'add') {
      $heap->{touched}->{$_}++ foreach (@which);
    }
    elsif ($op eq 'del') {
      delete $heap->{touched}->{$_} foreach (@which);
    }
    elsif ($op eq 'wipe') {
      $heap->{touched} = { };
    }
  }

  elsif ($event eq CM_TOUCHED) {
    $text = 'TOUCHED:' . ($argv[0] ? '' : ' (none)');
  }

  # Otherwise, what is it?
  else {
    $text = "Unknown event <$event> with values <@argv>";
  }

  if (defined $text) {
    if (exists $heap->{wheel}) {
      if (ref($text) eq 'ARRAY') {
        $heap->{wheel}->put(@$text);
      }
      else {
        $heap->{wheel}->put($text);
      }
    }
    else {
      warn( ",-----\n",
            "| Attempt to put text to nonexistent wheel:\n",
            "| $text\n",
            "`-----\n"
          );
    }
  }
}

#------------------------------------------------------------------------------

# Handle errors thrown by the socket read/write wheel.
sub session_error {
  my ($session, $heap, $operation, $errnum, $errstr) =
    @_[SESSION, HEAP, ARG0..ARG2];
  warn('Session ', $session->ID, " got $operation error $errnum: $errstr\n")
    if ($errnum);
  delete $heap->{wheel};
}

# The output buffer has been flushed to a socket.  If we're shutting
# down, now's the time to stop.
sub session_flushed {
  my $heap = $_[HEAP];
  delete $heap->{wheel} if $heap->{done};
}

# Finally, handle input.  This is where most of the interface work
# happens.
sub session_input {
  my ($kernel, $session, $heap, $line) = @_[KERNEL, SESSION, HEAP, ARG0];

  $line =~ s/^\s+//;
  $line =~ s/\s+$//;

  if ($line =~ /[\x00-\x1F\x7F\xFF]/) {
    $kernel->yield(CHEV_EVENT, CM_BAD_INPUT, $session);
    return;
  }

  if ($line =~ /^zuul\s*(.+)/i) {
    my $test = md5_base64($1);
    if ($test eq $heap->{password}) {
      $kernel->yield(CHEV_EVENT, CM_ADMIN_OK, $session);
      $heap->{is_a_god} = 1;
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_ADMIN_BAD, $session);
    }
    return;
  }

  if ($line =~ /^load\s*(\S+)/i) {
    unless ($heap->{is_a_god}) {
      $kernel->yield(CHEV_EVENT, CM_NOT_GOD, $session);
      return;
    }

    if ($heap->{crossword}->load($1)) {
      $kernel->post(chat => plex => CM_LOAD_OK, $1);
      $kernel->call(chat => plex => CM_TOUCH, 'wipe');
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_LOAD_BAD, $session, $1, $!);
    }
    return;
  }

  if ($line =~ /^pwd$/i) {
    unless ($heap->{is_a_god}) {
      $kernel->yield(CHEV_EVENT, CM_NOT_GOD, $session);
      return;
    }

    $kernel->yield(CHEV_EVENT, CM_PWD, $session, cwd);
    return;
  }

  if ($line =~ /^ls\s*(.*)$/i) {
    unless ($heap->{is_a_god}) {
      $kernel->yield(CHEV_EVENT, CM_NOT_GOD, $session);
      return;
    }

    my $directory = (defined($1) and length($1)) ? $1 : '.';
    if (opendir DIR, $directory) {
      my @files = sort readdir DIR;
      closedir DIR;
      foreach (@files) {
        if (-d $_) { $_ .= '/'; next; }
        if (-l $_) { $_ .= '@'; next; }
        if (-p $_) { $_ .= '|'; next; }
        if (-S $_) { $_ .= '='; next; }
        if (-x $_ or -X $_) { $_ .= '*'; next; }
      }
      if (@files) {
        $kernel->yield(CHEV_EVENT, CM_DIR_FILES, $session, $1, @files);
      }
      else {
        $kernel->yield(CHEV_EVENT, CM_DIR_EMPTY, $session);
      }
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_DIR_BAD, $session, $1, $!);
    }
    return;
  }

  if ($line =~ /^nick\s+(.+)$/i) {
    $kernel->call(chat => nick => $1);
    return;
  }

  if ($line =~ /^quit$/i) {
    $heap->{done} = 1;
    $heap->{wheel}->event( InputState => undef );
    $kernel->yield(CHEV_EVENT, CM_QUIT, $session);
    return;
  }

  if ($line =~ /^(\"\s*|say\s+)(.+)\s*$/i) {
    $kernel->call(chat => say => $2);
    return;
  }

  if ($line =~ /^(\:\s*|\/me\s+|do\s+)(.+)\s*$/i) {
    $kernel->call(chat => emote => $2);
    return;
  }

  if ($line =~ /^who$/i) {
    $kernel->yield( CHEV_EVENT, CHMT_WHO,
                    $session, @{$kernel->call(chat => 'who')}
                  );
    return;
  }

  if ($line =~ /^(\?|help)\s*(.*)$/i) {
    $kernel->yield(CHEV_EVENT, CM_HELP, $session, $2);
    return;
  }

  if ( $line =~ /^version$/i ) {
    $kernel->yield(CHEV_EVENT, CM_VERSION, $session);
    return;
  }

  if ( $line =~ /^uptime$/i ) {
    $kernel->yield(CHEV_EVENT, CM_UPTIME, $session);
    return;
  }

  if ($line =~ /^admin\s*help$/i) {
    unless ($heap->{is_a_god}) {
      $kernel->yield(CHEV_EVENT, CM_NOT_GOD, $session);
      return;
    }

    $kernel->yield(CHEV_EVENT, CM_ADMIN_HELP, $session);
    return;
  }

  if ($line =~ /^(shutdown|restart)$/i) {
    my $requested_type = $1;

    unless ($heap->{is_a_god}) {
      $kernel->yield(CHEV_EVENT, CM_NOT_GOD, $session);
      return;
    }

    $shutdown_type = $requested_type;
    $kernel->signal($_[KERNEL], 'KILL');
    return;
  }

  if ($line =~ /^grid$/i) {
    my ($status, @grid) = $heap->{crossword}->get_grid();
    if ($status == XWS_SUCCESS) {
      if ($heap->{has_ansi}) {
        foreach (@grid) {
          # Numbers are green.
          s/([0-9]+)/\e\[0;32m$1/g;
          # Letters are bright white.
          s/([A-Z]+)/\e\[1;37m$1/g;
          # Filled cells are brown.
          s/(\#+)/\e\[0;33m$1/g;
          # Everything else is cyan.
          s/([\|\_\-]+)/\e\[0;36m$1/g;
        }
        $grid[-1] .= "\e[0m";
      }
      $kernel->yield(CHEV_EVENT, CM_SHOW_GRID, $session, @grid);
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
    }
    return;
  }

  if ($line =~ /^tries\s+(\d+\s*[ad])$/i) {
    $kernel->yield(CHEV_EVENT, CM_TRIES, $session, $1);
    return;
  }

  if ($line =~ /^(all\s*)?across$/i) {
    my ($status, @clues) = $heap->{crossword}->get_clues('a', defined $1);
    if ($status == XWS_NO_PUZZLE) {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
      return;
    }
    $kernel->yield(CHEV_EVENT, CM_ACROSS, $session, @clues);
    return;
  }

  if ($line =~ /^(all\s*)?down$/i) {
    my ($status, @clues) = $heap->{crossword}->get_clues('d', defined $1);
    if ($status == XWS_NO_PUZZLE) {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
      return;
    }
    $kernel->yield(CHEV_EVENT, CM_DOWN, $session, @clues);
    return;
  }

  if ($line =~ /^(all\s*)?both$/i) {
    my ($status, @clues) = $heap->{crossword}->get_clues('a', defined $1);
    if ($status == XWS_NO_PUZZLE) {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
      return;
    }
    $kernel->yield(CHEV_EVENT, CM_ACROSS, $session, @clues);

    ($status, @clues) = $heap->{crossword}->get_clues('d', defined $1);
    $kernel->yield(CHEV_EVENT, CM_DOWN, $session, @clues);

    return;
  }

  if ($line =~ /^(clue\s+)?(\d+\s*[ad]?)$/i) {
    my $clue_id = lc $2;
    $clue_id =~ tr[a-zA-Z0-9][]cd;

    my ($status, @clues) = $heap->{crossword}->get_clue_by_id($clue_id);

    if ($status == XWS_SUCCESS) {
      $kernel->yield(CHEV_EVENT, CM_CLUE, $session, @clues);
    }
    elsif ($status == XWS_NO_PUZZLE) {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
    }
    elsif ($status == XWS_NO_CLUE) {
      $kernel->yield(CHEV_EVENT, CM_NO_CLUE, $session, $clue_id);
    }
    return;
  }

  if ($line =~ /^(\d+\s*[ad])\s+is\s+(.+)$/i) {
    my ($clue_id, $word) = (lc($1), uc($2));
    $clue_id =~ tr[a-zA-Z0-9][]cd;
    $word    =~ tr[a-zA-Z][]cd;

    my $my_nick = $kernel->call(chat => get_nick => $session);
    my ($status, $got, $add, $del) =
      $heap->{crossword}->answer($my_nick, $clue_id, $word);

    if ($status == XWS_SUCCESS) {
      $kernel->post(chat => plex => CM_GOT_WORD, $got, $add);
      $kernel->call(chat => plex => CM_TOUCH, 'add', @$add) if @$add;
      $kernel->call(chat => plex => CM_TOUCH, 'del', @$del) if @$del;
    }
    elsif ($status == XWS_NO_CLUE) {
      $kernel->yield(CHEV_EVENT, CM_NO_CLUE, $session, $clue_id);
    }
    elsif ($status == XWS_NO_PUZZLE) {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
    }
    elsif ($status == XWS_WRONG_SIZE) {
      $kernel->yield( CHEV_EVENT, CM_WRONG_SIZE,
                      $session, $clue_id, $word, $got
                    );
    }
    elsif ($status == XWS_ALREADY_DONE) {
      $kernel->yield( CHEV_EVENT, CM_ANSWER_DONE,
                      $session, $clue_id, $got
                    );
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_GOT_NONE, $session, $clue_id, $word);
    }
    return;
  }

  if ($line =~ /^touch$/i) {
    my @touched = keys %{$heap->{touched}};
    $heap->{touched} = { };
    $kernel->yield(CHEV_EVENT, CM_TOUCHED, $session, scalar @touched);
    foreach my $touched ($heap->{crossword}->sort_clue_ids(@touched)) {
      my ($status, @clues) = $heap->{crossword}->get_clue_by_id($touched);
      if ($status == XWS_SUCCESS) {
        $kernel->yield(CHEV_EVENT, CM_CLUE, $session, @clues);
      }
      else {
        $kernel->yield(CHEV_EVENT, CM_NO_CLUE, $session, $touched);
      }
    }
    return;
  }

  if ($line =~ /^about$/i) {
    my ($status, @headers) = $heap->{crossword}->get_headers();
    if ($status == XWS_SUCCESS) {
      $kernel->yield(CHEV_EVENT, CM_ABOUT, $session, @headers);
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
    }
    return;
  }

  if ($line =~ /^notes$/i) {
    my ($status, @notes) = $heap->{crossword}->get_notes();
    if ($status == XWS_SUCCESS) {
      $kernel->yield(CHEV_EVENT, CM_NOTES, $session, @notes);
    }
    elsif ($status == XWS_NO_PERMISSION) {
      $kernel->yield(CHEV_EVENT, CM_NO_NOTES, $session);
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_NO_PUZZLE, $session);
    }
    return;
  }

  if ($line =~ /^ansi\s*(on|off)$/i) {
    if ($1 eq 'on') {
      $kernel->yield(CHEV_EVENT, CM_ANSI, $session, $heap->{has_ansi} = 1);
      return;
    }
    else {
      $kernel->yield(CHEV_EVENT, CM_ANSI, $session, $heap->{has_ansi} = 0);
      return;
    }
  }

  $kernel->yield(CHEV_EVENT, CM_WHAT, $session);
}

###############################################################################
# Main loop.  Create a server, and run it.  When the server is done,
# check whether we should restart.  If we're restarting, just exec
# another copy of us in-place.

package main;

# Required values in xws.conf.
my ($admin_passcode, $listen_port);

# Read the xws.conf file.
open XWS, "<$xws_path/xws.conf" or die "could not read $xws_path/xws.conf";
while (<XWS>) {
  chomp;
  next if /^\s*\#/;
  next unless /^(\S+)\s+(.*?)\s*$/;
  my ($tag, $value) = ($1, $2);

  if ($tag eq 'admin_passcode') {
    $admin_passcode = $value;
  }
  elsif ($tag eq 'listen_port') {
    $listen_port = $value;
  }
  else {
    warn "$0: unknown option '$tag' at $xws_path/xws.conf line $.\n";
  }
}
close XWS;

die "$0: $xws_path/xws.conf did not set admin_passcode\n"
  unless defined $admin_passcode;

die "$0: $xws_path/xws.conf did not set listen_port\n"
  unless defined $listen_port;

# Create a static crossword instance that everyone will share.
# Because POE is all event driven, it guarantees serial access to the
# puzzle.  Because it's objectified, we could probably hand each
# client session an individual puzzle.  Madness may ensue.
my $cw = Puzzle::Crossword::AcrossLite->new();

# Create a chat multiplexer.  It's static, but it's addressed by name.
POE::Component::Chat->new( 'chat' );

# Create the text UI server.
new POE::Component::Server::TCP
  ( Port => $listen_port,
    Acceptor =>
    sub {
      my ($socket, $remote_address, $remote_port) = @_[ARG0..ARG2];
      new UI::Crossword::CLI( $cw, $admin_passcode,
                              $socket, $remote_address, $remote_port
                            );
    },
  );

# Run things 'til they stop.
$poe_kernel->run();

if ($shutdown_type eq 'restart') {
  exec $xws_program;
  die "Could not exec $xws_program: $!";
}

exit;

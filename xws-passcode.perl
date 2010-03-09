#!/usr/bin/perl -w
# $Id$

# Generate a new passcode for xws.conf.  It doesn't change xws.conf
# for you, though.

use strict;
use Digest::MD5 qw(md5_base64);

print <<EOT;

,-----------------------------------------------------------
| Greetings!  Please enter a new xws passcode.  This is a
| cheezy hack, so it will echo what you type.
`-----------------------------------------------------------

EOT

my $passcode_one = my $passcode_two = '';
while ('true') {

  while ($passcode_one eq '') {
    print "Please enter a new passcode: ";
    $passcode_one = <STDIN>;
    chomp $passcode_one;
  }

  while ($passcode_two eq '') {
    print "Please retype the new passcode: ";
    $passcode_two = <STDIN>;
    chomp $passcode_two;
  }

  last if $passcode_two eq $passcode_one;

  print <<"  EOT";

,-----------------------------------------------------------
| I'm sorry, but the passcodes you entered don't match.
| Rather than use one or the other and possibly surprise
| you later, I'm afraid you'll have to enter them again.
`-----------------------------------------------------------

  EOT

  $passcode_one = $passcode_two = '';
}

my $encoded_passcode = md5_base64 $passcode_one;
print <<EOT;
,-----------------------------------------------------------
| Great!  Your passcode looks like this after MD5 encoding:
|
|         $encoded_passcode
|
| You should edit xws.conf and replace the current encoded
| passcode with this new one.  It's on the admin_passcode
| line.  The new admin passcode will be in effect the next
| time xws.perl is started.
`-----------------------------------------------------------
EOT

exit;

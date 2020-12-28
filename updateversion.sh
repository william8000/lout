#!/bin/sh
#
# updateversion.sh -- update the lout version before a release
#
#    usage: updateversion.sh new-version
#

NEW="$1"

if [ -z "$NEW" ]
then
	echo "$0: Syntax: new-version"
	exit
fi

case "$NEW" in
[0-9].[0-9][0-9]) ;;
*) echo "$0: Error: old-version $NEW should be #.##" ; exit ;;
esac

if [ ! -f "z01.c" ] || [ ! -f externs.h ]
then
	echo "$0: This does not look like a lout source directory"
	exit
fi

echo "Updating the version strings to $NEW"

temp=temp

# Update the line: /*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION X.XX)                       */

for name in *.c *.h makefile README
do
	if [ ! -f "$name" ]
	then
		echo "$0: Error: $name not found"
		continue
	fi
	rm -f "$temp"
	if sed -e "s/VERSION [0-9]\.[0-9][0-9]/VERSION $NEW/" < "$name" > "$temp"
	then
		mv -f "$name" "${name}-"
		mv "$temp" "$name"
	else
		echo "$0: Error processing $name"
	fi
done

rm -f "$temp"

# Update the line: #define	LOUT_VERSION   AsciiToFull("Basser Lout Version X.XX (Mmm YYYY)")

name=externs.h
if sed -e "/LOUT_VERSION/s/[0-9]\.[0-9][0-9].*\"/$NEW ($(date +'%b %Y'))\"/" < "$name" > "$temp"
then
	mv -f "$name" "${name}-"
	mv "$temp" "$name"
else
	echo "$0: Error processing $name"
fi

rm -f "$temp"

# Update the line: VERSION = X.XX

name=makefile
if sed -e "/^VERSION/s/[0-9]\.[0-9][0-9]/$NEW/" < "$name" > "$temp"
then
	mv -f "$name" "${name}-"
	mv "$temp" "$name"
else
	echo "$0: Error processing $name"
fi

rm -f "$temp"

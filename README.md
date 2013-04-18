Spaceman
========

![Spaceman](https://github.com/wcauchois/spaceman/raw/master/logo.png)

Spaceman (short for "space manager") is a dead-simple in-memory spatial cache.

Protocol
--------

**PUT (x, y, label)**

> Associate label with the point at (x, y). The result with either be "OK" or "ERROR <message>".

**RETRIEVE {x, y, width, height}**

> Retrieve all points and their associated labels within an area. The result will be a tab delimited table in the form of
> repeated "x\ty\tlabel" entries.

**DELETE label**

> Delete an entry by its label. The result will either be "OK" or "ERROR <message>".

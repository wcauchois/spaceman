Spaceman
========

![Spaceman](https://github.com/wcauchois/spaceman/raw/master/logo.png)

Spaceman (short for "space manager") is a dead-simple in-memory spatial cache.

Protocol
--------

**PUT (x, y, label)**

> Associate _label_ with the point at (_x_, _y_). The result will either be **OK** or **ERROR** _string_.

**RETRIEVE {x, y, width, height}**

> Retrieve all points and their associated labels within an area. The result will be
> a table in the form of repeated _x_**\t**_y_**\t**_label_**\n** entries.

**DELETE label**

> Delete an entry by its label. The result will either be **OK** or **ERROR** _string_.

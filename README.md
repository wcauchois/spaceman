Spaceman
========

Spaceman (short for "space manager") is a dead-simple in-memory spatial cache.

Protocol
--------

**PUT (**_x_**,**
_y_**)**
_label_

> Associate _label_ with the point at (_x_, _y_). The result will either be **OK** or **ERROR** _string_.

**RETREIVE {**_x_**,**
_y_**,**
_width_**,**
_height_**}**

> Retrieve all points and their associated labels within an area. The result will be
> a table in the form of repeated _x_**\t**_y_**\t**_label_**\n** entries.

**DELETE**
_label_

> Delete an entry by its label. The result will either be **OK** or **ERROR** _string_.

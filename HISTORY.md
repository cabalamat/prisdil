# HISTORY

Change Log from:

    type Round = (Move,Move)
    type Log = List[Round]

to:

    type Log = (List[Move],List[Move])

for efficiency.

(end.)

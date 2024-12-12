module SaveTestsData exposing (..)

{--| Note that if your JSON contains backslashes you need to double them: \ → \\. You can do that with find-and-replace.
-}


v0_1Example1 : String
v0_1Example1 =
    """
{"version":"v1","currentTime":1733347930211,"level":1,"credits":0,"resources":{"gold":0},"missionStatuses":{"haz1":null,"haz2":null,"haz3":null,"haz4":null,"haz5":null}}
        """


v1Example2 : String
v1Example2 =
    """
{"version":"v1","currentTime":1733356012211,"level":1,"credits":3.75,"resources":{"gold":0},"missionStatuses":{"haz1":{"current":{"current":0.025533333333333314}},"haz2":null,"haz3":null,"haz4":null,"haz5":null}}
        """


v2Examples : List String
v2Examples =
    [ """
{"version":"v2","currentTime":1733356361259,"currentTab":"Missions","theme":"default","level":6,"credits":3.017500000000003,"resources":{"gold":0},"missionStatuses":{"haz1":null,"haz2":{"current":{"current":0.00934333333333334,"hasEverTicked":true}},"haz3":null,"haz4":null,"haz5":null},"dwarfXpButtonStatuses":{"dwarfXpButton1":null,"dwarfXpButton2":{"current":0.014593333333333323,"hasEverTicked":true},"dwarfXpButton3":null,"dwarfXpButton4":null,"dwarfXpButton5":null},"dwarfXp":{"scout":2,"gunner":0,"engineer":0,"driller":0}}
"""
    ]

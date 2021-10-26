1. Fækka breytum sem eru fáfengilegar eða rýrar
    - undirmatssvæði, lyftu og (ibteg / teg_eign) má mögulega einfalda
    - rfastnum og svfn má taka út
    - fjeld og fjbilast má mögulega taka út
2. Skoða collinearity milli tölulegra breyta
    - t.d. eigingildi (15: "ibm2"  "fjhaed"  "fjsturt"  "fjklos"  "fjherb"  "fjstof")
    - fjarlægja þá þætti sem minnstu máli virðast skipta
    - getum skoðað heatmap líka
3. Skrá fyrsta módelið og hvernig því gengur, geyma allar upplýsingar.
    - R^2, PRESS, etc.
    - mean, variance, min/max, etc.
4. Þegar við erum komnir með sæmilegt grunnmódel, áður en við förum í transformations:
    - Skoða útlaga í X og y punktum
        - Mahalanobis / H_ii á X punkta
        - Cook's distance og jackknife á y punkta
    - Skoða QQ plot fyrir dreifingu leifðar, sjá hvort hún sé línuleg
        - ef ekki, skoða transformation á y
    - Greina heteroskedacity
        - ef já, skoða transformation á y
    - Skoða partial regression / residuals á öllum X-breytum
        - í bili, bara nótera hjá okkur hverjar stinga í stúf
        - eða, ef þær eru fáar, skoða transformation á þeim
5. Transformations
    - Má fjarlægja x og setja x^2 í staðinn?
        - eða þarf x alltaf að vera inni á línulegu formi?
    - BoxCox á y breytuna og sjá hvernig hún breytist
    -   BoxCox næm fyrir útlögum, þess vegna skoða að fjarlægja þannig punkta fyrst
    -   Uppfæra ef tilefni þykir
    -   Muna að taka f^-1 af breytunum fyrir skekkju etc.
6. Partial regression / residuals á X breytur
    - Verður ótrúlega stórt fyrir safnið í núverandi mynd
    - TukeyHSD er bara fyrir ANCOVA.
    - Er Bonferroni málið fyrir þetta?
7. Vinnsla með flokkunarfræðilegar breytur:
    - Bíða með þær þangað til hitt hefur náð sæmilegri nákvæmni
8. Skrá PRESS, RMSE, R^2, Cp stat fyrir hvert módel
    - plotta muninn fyrir ólíkar breytur
    - prófa að taka út breytur með hátt p-gildi
        - prófa eina í einu og fikra okkur þannig áfram?
9. Getum prófað að keyra STEPWISE selection á allt safnið í byrjun
    - Notum það ekki en getum haft það sem viðmið
10. Endurtaka frá skrefi 4, skrá hjá okkur helstu upplýsingar en ekki nauðsynlegt.



### Spurningar

- Sleppa kaupdegi?
- Ath, eru einhver auð gildi?


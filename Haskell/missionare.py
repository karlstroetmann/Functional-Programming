# Die Bekehrung der Ungläubigen

# Drei Missionare und drei Ungläubige wollen zusammen einen Fluss überqueren, denn die Ungläubigen sollen
# in der Kirche, die sich auf dem anderen Ufer befindet, getauft werden. Sie haben nur ein Boot, das Platz
# für **maximal zwei**  Passagiere bietet.  Sowohl die Ungläubigen als auch die Missionare können rudern.
# Es ist zusätzlich bekannt, dass die Ungläubigen zum Kannibalismus neigen. Die Kannibalen sind hungrig,
# wenn die Missionare an einem der beiden Ufer in der Unterzahl sind, haben sie ein **Problem**.  Ihre
# Aufgabe besteht darin, einen Fahrplan zu erstellen, so dass einerseits alle Personen das andere Ufer
# erreichen und andererseits die Missionare zwischendurch kein Problem bekommen. 

def problem(m, k):
    """
    This function returns True if there is a problem on a shore that has 
    m missionaries and k cannibals.
    """
    return 0 < m < k

States = { (m, k, b) for m in range(0, 4)
                     for k in range(0, 4)
                     for b in [0, 1]
                     if not problem(m, k) and not problem(3-m, 3-k)
         }

def boatOK(mb, kb):
    return 1 <= mb + kb <= 2

R1 = { ((m, k, 1), (m - mb, k - kb, 0)) for (m, k, b) in States
                                        for mb in range(m+1)
                                        for kb in range(k+1)
                                        if (m-mb, k-kb, 0) in States
                                        if boatOK(mb, kb)
     }

R2 = { (y, x) for (x, y) in R1 }

R = R1 | R2 

start = (3, 3, 1)
goal  = (0, 0, 0)
Path  = search(R, start, goal)


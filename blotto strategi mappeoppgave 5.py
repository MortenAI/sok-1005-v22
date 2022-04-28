import numpy as np

def player_strategy(n_battalions,n_fields):
    
    battalions=np.zeros(n_fields,dtype=int)
    
    #Setter 30 battaljoner på første runde, 30 på andre, 30 på tredje, 8 på fjerde, 1 på femte, 1 på sjette
    battalions[0:]=30
    battalions[1:]=30
    battalions[2:]=30
    battalions[3:]=8
    battalions[4:]=1
    battalions[5:]=1

    battalions=battalions[np.random.rand(n_fields).argsort()]
    assert sum(battalions)==n_battalions
    
    return battalions
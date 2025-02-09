# Front-end built 

## Tool stack
Build: Vite
UI: React + CSS 
Querying: Tanstack
Caching: IDB

Local-only state managed with vanilla React. 
Shared state managed with tanstack. 

Offline work enabled by IndexedDB. Tanstack mutations try requests, and on error puts the attempted mutation in db. 

Only the useQuery, not the useMutations, needs to setState the "actualOnline" to false on request fail, because all the mutations will trigger useQuery with invalidateQuery anyway. 

Sync: First fetch from server, then make posts, then make puts, then make deletes. 
All query/mutation functions should first check if there is cache and if yes try sync before their regular request. 

function (newData) => {
    if (anyUnSynced) {
        trySync() //ends with setting anyUnSynced=True
    }
    doQuery()
        .catch((newData) => cache)
}

import { netGetTodos } from "./networking";
import { cacheTodoList } from "./databasing";

const mutationFunction = async (todo, netFunction, dbFunction) => {
    return await netFunction(todo)
        .then(async () => {await dbFunction({...todo, knownUnSynced: false})})
        .catch(async (error) => {await dbFunction(todo)})
};

const cacheServerTodos = async () => {
    const serverTodos = await netGetTodos()
        .then((responseJson) => {
            cacheTodoList(responseJson);
        })
}

export {mutationFunction, cacheServerTodos};
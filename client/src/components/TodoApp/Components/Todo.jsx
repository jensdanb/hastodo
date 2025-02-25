import {useState} from "react"; 
import { useMutation } from "@tanstack/react-query";
import { netPutTodo } from "../../../services/networking";
import { dbPutTodo } from "../../../services/databasing";
import { mutationFunction } from "../../../services/common";

function Todo(props) {
    
    const [isEditing, setIsEditing] = useState(false);
    
    const putTodoMutation = useMutation({
        mutationFn: async (newTask) => {
            return await mutationFunction(newTask, netPutTodo, dbPutTodo);
        },
        onSettled: props.invalidateTodoList
    })
        
    if (isEditing) {
        return <EditingTodo 
                    props={props} 
                    submitEdit={putTodoMutation.mutate}
                    setIsEditing={setIsEditing}
                />;
    } else 
        return <ViewTodo 
                    props={props} 
                    toggleTaskCompleted={putTodoMutation.mutate}
                    setIsEditing={setIsEditing}
                />;
  }

function EditingTodo({props, submitEdit, setIsEditing}) {

    const [newName, setNewName] = useState(props.name);

    function handleTyping(event) {
        setNewName(event.target.value);
    }

    function submitEditValid(event) {
        event.preventDefault();
        if (newName != "") {
            const newTask = { ...props.todoData, name: newName, knownUnSynced: true };
            submitEdit(newTask);
            setIsEditing(false);
        };
    }

    return (
        <form className="todo stack-small" onSubmit={submitEditValid}>
            <div className="form-group">
                
                
                <input 
                    id={props.id}
                    className="todo-text"
                    type="text" 
                    value={newName}
                    onChange={handleTyping}
                />
                
            </div>
            
            <div className="btn-group">
                <button type="button" className="btn" onMouseDown={() => setIsEditing(false)}>
                    Cancel 
                    <span className="visually-hidden">renaming {props.name}</span>
                </button>
                
                <button type="submit" className="btn btn__strong">
                    Save 
                    <span className="visually-hidden">new name for {props.name}</span>
                </button>
            </div>
        
        </form>
        );
}

function ViewTodo({props, toggleTaskCompleted, setIsEditing}) {
    return (
        <div className="todo stack-small">
            <div className="c-cb">
                <input 
                    id={props.id} 
                    type="checkbox" 
                    checked={props.completed} 
                    onChange={() => toggleTaskCompleted({ ...props.todoData, completed: !props.completed, knownUnSynced: true })}
                />
                
                <label className="todo-label" htmlFor={props.id}>
                    {props.name}
                </label>
            </div>
            
            <div className="btn-group">
                <button type="button" className="btn" onMouseDown={() => setIsEditing(true)} >
                    Edit 
                    <span className="visually-hidden">{props.name}</span>
                </button>
                
                <button 
                    type="button" 
                    className="btn btn__strong" 
                    onMouseDown={() => props.deleteTask(props.id)}
                    >
                    Delete 
                    <span className="visually-hidden">{props.name}</span>
                </button>
            </div>
        
        </div>
        );
}

  
export default Todo;
  
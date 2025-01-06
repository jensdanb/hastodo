import {useState} from "react"; 
import { useMutation } from "@tanstack/react-query";
import { putTodo } from "../../../services/networking";

function Todo(props) {
    
    const [isEditing, setIsEditing] = useState(false);
    
    const putTodoMutation = useMutation({
            mutationFn: putTodo,
            onSettled: props.invalidateTodoList,
            mutationKey: ['putTodo']
        })
    const { isPending, submittedAt, variables, mutate, isError } = putTodoMutation;
        
    if (isEditing) {
        return <EditingTodo 
                    props={props} 
                    submitEdit={putTodoMutation.mutate}
                    setIsEditing={setIsEditing}
                />;
    } else 
        return <ViewTodo 
                    props={props} 
                    toggleTaskCompleted={mutate}
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
            submitEdit({id: props.id, toggle: false, newName: newName});
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
                    onChange={() => toggleTaskCompleted({id: props.id, toggle: true, newName: props.name})}
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
  
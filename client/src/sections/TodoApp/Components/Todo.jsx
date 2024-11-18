import {useState} from "react"; 

function Todo(props) {

    const [isEditing, setIsEditing] = useState(false);

    if (isEditing) {
        return <EditingTodo props={props} setIsEditing={setIsEditing}/>;
    } else return <ViewTodo props={props} setIsEditing={setIsEditing}/>;
  }

function EditingTodo({props, setIsEditing}) {

    const [newName, setNewName] = useState(props.name);

    function handleTyping(event) {
        setNewName(event.target.value);
    }

    function submitEdit(event) {
        event.preventDefault();
        if (newName != "") {
            props.editTask(props.id, newName);
            setIsEditing(false);
        };
    }

    return (
        <form className="todo stack-small" onSubmit={submitEdit}>
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

function ViewTodo({props, setIsEditing}) {
    return (
        <div className="todo stack-small">
            <div className="c-cb">
                <input 
                    id={props.id} 
                    type="checkbox" 
                    defaultChecked={props.completed} 
                    onChange={() => props.toggleTaskCompleted(props.id)}
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
  
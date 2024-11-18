import {useState} from "react"; 

function Form(props) {
    
    const [name, setName] = useState("");

    function handleTyping(event) {
        setName(event.target.value);
    }

    function handleSubmit(event) {
        event.preventDefault();
        if (name != "") {
            props.onSubmit(name);
            setName("");
        };
    }

    return (
      <form 
      onSubmit={handleSubmit}
      >

        <h2 className="label-wrapper">
          <label htmlFor="new-todo-input" className="label__lg">
            What has to be done?
          </label>
        </h2>

        <input
          type="text"
          id="new-todo-input"
          className="input input__lg"
          name="text"
          autoComplete="on"
          value={name}
          onChange={handleTyping}
        />

        <button type="submit" className="btn btn__strong btn__lg">
          Add
        </button>
      </form>
    );
  }
  
  export default Form;
  

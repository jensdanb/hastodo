
function PwaController (props) {
    const onlineStatusMsg = props.intendedOnline ? "Online mode" : "Offline mode";
    return (
        <>
            <p>{onlineStatusMsg}</p>
            <button 
                type="button" 
                className="btn toggle-btn" 
                aria-pressed="true"
                onMouseDown={props.toggleOnline}>
                <span>Toggle online </span>
            </button>
        </>);
};

export default PwaController;

// if (!props.actualOnline) return <div>Tried to connect, but failed</div>;

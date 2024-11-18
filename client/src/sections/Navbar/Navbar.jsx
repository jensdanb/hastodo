const NavBar = () => {
    return <>
    <nav className="navbar">
        <ul className="navbar-nav me-auto mb-2 mb-lg-0">
            <li className="nav-item">
                <a className="nav-link active" aria-current="page" href="/">
                    Home
                </a>
            </li>
            <li className="nav-item">
                <a className="nav-link" href="https://google.com">
                    Google.com
                </a>
            </li>
            <li className="nav-item dropdown">
                <a className="nav-link dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown" aria-expanded="false">
                    Dropdown
                </a>
            </li>
            <li className="nav-item">
                <a className="nav-link disabled" aria-disabled="true">
                    Disabled
                </a>
            </li>
        </ul>
    </nav>
    </>
}

export default NavBar;
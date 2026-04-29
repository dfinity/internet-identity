import { Link, Route, Routes } from "react-router-dom";

import { Home } from "./pages/Home";
import { Issues } from "./pages/Issues";
import { AdminLanding } from "./pages/AdminLanding";
import { AdminPanel } from "./pages/AdminPanel";

export function App() {
  return (
    <div className="app">
      <main>
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="/issues" element={<Issues />} />
          <Route path="/admin" element={<AdminLanding />} />
          <Route path="/admin/panel" element={<AdminPanel />} />
        </Routes>
      </main>
      <footer>
        <Link to="/admin" className="admin-link">
          Admin
        </Link>
      </footer>
    </div>
  );
}

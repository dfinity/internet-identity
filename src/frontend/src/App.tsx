import React from "react";
import { Container, Typography } from "@material-ui/core";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import AddIdentity from "./components/AddIdentity";
import LookupIdentity from "./components/LookupIdentity";
import RegisterIdentity from "./components/RegisterIdentity";
import RemoveIdentity from "./components/RemoveIdentity";
import logo from "../assets/logo.png";

const routes = {
  home: "/",
};
export function App() {
  return (
    <BrowserRouter>
      <Switch>
        <Route path={routes.home}>
          <main>
            <Container>
              <picture>
                <img src={logo} alt="dfinity logo" />
              </picture>
              <Typography variant={"h1"}>IDP service</Typography>
              <RegisterIdentity />
              <AddIdentity />
              <LookupIdentity />
              <RemoveIdentity />
            </Container>
          </main>
        </Route>
      </Switch>
    </BrowserRouter>
  );
}

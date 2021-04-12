import React from "react";
import { Container, Typography } from "@material-ui/core";
import { BrowserRouter, Route, Switch } from "react-router-dom";

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
              <Typography variant={"h1"}>IDP service</Typography>
            </Container>
          </main>
        </Route>
      </Switch>
    </BrowserRouter>
  );
}

import React from 'react';
import ReactDOM from 'react-dom';

export const reactView = (): void => {
    document.title = "REACT! | Internet Identity";
    const container = document.getElementById("pageContent") as HTMLElement;

    ReactDOM.render(<div></div>, container);

}

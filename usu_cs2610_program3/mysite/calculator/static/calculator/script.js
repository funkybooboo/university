const submit = document.getElementById("submit")
const container = document.getElementById("container");
const token = document.getElementById("token").firstElementChild;
let recordPath = document.getElementById("recordPath").value;

submit.addEventListener('click', function () {
    let lastID = document.getElementById("lastID").value;
    let newToken = token.cloneNode(true)
    const num1 = document.getElementById("num1").value;
    const operator = document.getElementById("operator").value;
    const num2 = document.getElementById("num2").value;
    let answer = "";
    let expression = "";

    if (!Number.isNaN(Number.parseFloat(num1)) && !Number.isNaN(Number.parseFloat(num2)) && ['+', '-', '*', '/', '%', '**'].includes(operator)) {
        if (num2.toString() === "0" && operator.toString() === "/") {
            answer = "Error - divide by 0 not possible";
        } else {
            expression = num1.toString() + " " + operator.toString() + " " + num2.toString();
            answer = eval(expression.toString());
        }
    } else if (num1.toString() === "" || num2.toString() === "") {
        answer = "Error - empty fields";
    } else if (Number.isNaN(Number.parseFloat(num1)) || Number.isNaN(Number.parseFloat(num2))) {
        answer = "Error - answer is not a number";
    } else if (!['+', '-', '*', '/', '%', '**'].includes(operator)) {
        answer = "Error - invalid operator";
    } else {
        answer = "Error";
    }

    const num1HiddenInput = document.createElement("input");
    num1HiddenInput.setAttribute("type", "hidden");
    num1HiddenInput.setAttribute("name", "num1");
    num1HiddenInput.setAttribute("value", num1);

    const operatorHiddenInput = document.createElement("input");
    operatorHiddenInput.setAttribute("type", "hidden");
    operatorHiddenInput.setAttribute("name", "operator");
    operatorHiddenInput.setAttribute("value", operator);

    const num2HiddenInput = document.createElement("input");
    num2HiddenInput.setAttribute("type", "hidden");
    num2HiddenInput.setAttribute("name", "num2");
    num2HiddenInput.setAttribute("value", num2);

    const JSAwnserHiddenInput = document.createElement("input");
    JSAwnserHiddenInput.setAttribute("type", "hidden");
    JSAwnserHiddenInput.setAttribute("name", "js_answer");
    JSAwnserHiddenInput.setAttribute("value", answer);

    let expressionForm = document.createElement("form");
    expressionForm.setAttribute("action", recordPath);
    expressionForm.setAttribute("method", "post");
    expressionForm.setAttribute("id", "expression"+lastID);

    let mathSpan = document.createElement("span");
    mathSpan.setAttribute("class", "math");

    let saveInput = document.createElement("input");
    saveInput.setAttribute("class", "save");
    saveInput.setAttribute("type", "submit");
    saveInput.setAttribute("value", "Save");

    let deleteButton = document.createElement("button");
    deleteButton.setAttribute("class", "delete");
    deleteButton.setAttribute("value", "X");
    deleteButton.innerText = "X";
    deleteButton.addEventListener('click', function () {
        expressionForm.remove();
    });

    if (typeof(answer) === 'number' && Number.isFinite(answer)) {
        expressionForm.setAttribute("class", "valid");
        mathSpan.append(expression.toString()+" = "+answer.toString());
    } else if (typeof(answer) === 'undefined' || (operator === "/" && num2 === "0")) {
        expressionForm.setAttribute("class", "undefined");
        mathSpan.append(expression.toString()+" is undefined");
        saveInput.setAttribute("disabled", "");
    } else if (typeof(answer) === 'string') {
        expressionForm.setAttribute("class", "undefined");
        mathSpan.append(answer);
        saveInput.setAttribute("disabled", "");
    } else if (!Number.isFinite(answer)) {
        expressionForm.setAttribute("class", "infinite");
        mathSpan.append(expression.toString()+" = "+answer.toString());
        saveInput.setAttribute("disabled", "");
    } else if (Number.isNaN(answer)) {
        expressionForm.setAttribute("class", "NaN");
        mathSpan.append(expression.toString()+" = "+answer.toString());
        saveInput.setAttribute("disabled", "");
    }

    let saveInputSpan = document.createElement("span");
    saveInputSpan.append(saveInput)

    let deleteButtonSpan = document.createElement("span");
    deleteButtonSpan.append(deleteButton)

    expressionForm.append(num1HiddenInput);
    expressionForm.append(num2HiddenInput);
    expressionForm.append(operatorHiddenInput);
    expressionForm.append(JSAwnserHiddenInput);
    expressionForm.append(newToken);
    expressionForm.append(mathSpan);
    expressionForm.append(saveInputSpan);
    expressionForm.append(deleteButtonSpan);
    container.prepend(expressionForm);
    lastID = parseInt(lastID)+1;
    document.getElementById("lastID").value = lastID;
});

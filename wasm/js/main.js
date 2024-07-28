import { type_check, compile, disasm, compile_and_run } from "../pkg/index.js";


async function runCommon(process) {
    // Clear output
    const output = document.getElementById("output");
    output.value = "";
    const canvas = document.getElementById("canvas");
    const canvasRect = canvas.getBoundingClientRect();
    canvas.getContext("2d").clearRect(0, 0, canvasRect.width, canvasRect.height);

    const source = document.getElementById("input").value;
    const start = performance.now();
    try{
        process(source);
    }
    catch(e){
        output.value = e;
    }
    const end = performance.now();
    document.getElementById("timeMessage").innerHTML = `Execution time: ${(end - start).toFixed(1)} ms (See <a href="#Time">notes</a>)`;
}

document.getElementById("typeCheck").addEventListener("click", () => runCommon(source => {
    const result = type_check(source);
    document.getElementById("output").value = result;
}));
document.getElementById("compile").addEventListener("click", () => runCommon(source => {
    const result = compile(source);
    document.getElementById("output").value = result;
}));
document.getElementById("disasm").addEventListener("click", () => runCommon(source => {
    const result = disasm(source);
    document.getElementById("output").value = result;
}));
document.getElementById("compileAndRun").addEventListener("click", () => runCommon(source => {
    compile_and_run(source);
}));
document.getElementById("clearCanvas").addEventListener("click", () => {
    const canvas = document.getElementById("canvas");
    const canvasRect = canvas.getBoundingClientRect();
    canvas.getContext("2d").clearRect(0, 0, canvasRect.width, canvasRect.height);
});

document.getElementById("input").value = `
fn fact(n: i64) -> i64 {
    if n < i64(1) {
        1
    } else {
        n * fact(n - 1)
    }
}

print(fact(10));
`;

const samples = document.getElementById("samples");

[
    "fact.rscl",
    "fibonacci.rscl",
    "for.rscl",
    "nested_for.rscl",
    "type.rscl",
    "canvas.rscl",
]
    .forEach(fileName => {
    const link = document.createElement("a");
    link.href = "#";
    link.addEventListener("click", () => {
        fetch("scripts/" + fileName)
            .then(file => file.text())
            .then(text => document.getElementById("input").value = text);
    });
    link.innerHTML = fileName;
    samples.appendChild(link);
    samples.append(" ");
})

$(document).ready(function () {
    $("#highlight").html(delphiHighlighter.getHtml($("#resultText").val(), false, false, 9));
});

function CopyToClipboard() {
    var valueToCopy = resultText.getValue();

    function listener(e) {
        e.clipboardData.setData("text/html", valueToCopy);
        e.clipboardData.setData("text/plain", valueToCopy);
        e.preventDefault();
    }
    document.addEventListener("copy", listener);
    document.execCommand("copy");
    document.removeEventListener("copy", listener);


    var temp = $("#copy-to-clipboard-btn").html();
    $("#copy-to-clipboard-btn").text("Copied !");
    setTimeout(function () {
        $("#copy-to-clipboard-btn").html(temp);
    }, 1500);
};
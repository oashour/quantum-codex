function fixPreviewHeight(previewDiv) {
  const codexFile = previewDiv.closest(".container.codex-file");
  const inputFile = codexFile.find("div.input-file").first();
  const inputFileHeight = inputFile.height();

  const inputFileParent = inputFile.parents("div").first();
  const previewParent = previewDiv.parents("div").first();
  const inputFileParentHeight = inputFileParent.height();

  previewParent.height(inputFileParentHeight);
  previewParent.css({ maxHeight: inputFileParentHeight + "px" });
  previewDiv.height(inputFileHeight);
  previewDiv.css({ maxHeight: inputFileHeight + "px" });
}

window.onload = function () {
  // Hide the raw files on page load
  $(".container.file-container").hide();
  $('.container.file-container[data-displaytype="processed"]').show();
  // Fix the height of the preview pane to match the input file (on load)
  $("div.preview").each(function () {
    fixPreviewHeight($(this));
  });
};

$(document).ready(function () {
  // Adjust preview height on first accordion click
  $(".accordion-button").one("click", function () {
    const codexFile = $(this)
      .closest(".accordion-item")
      .find(".container.codex-file")
      .first();
    const previewDiv = codexFile.find("div.preview").first();
    fixPreviewHeight(previewDiv);
  });

  // Preview on hover
  $(".tag-link").hover(
    function () {
      const tag = $(this).data("name");
      const section = $(this).data("section");
      //const parent = $(this).parents("div.codex-file").first();
      const parent = $(this).closest(".container.codex-file");
      const cdxid = $(parent).data("cdxid");
      const dbversion = $(parent).data("dbversion");
      const code = $(parent).data("code");
      const filetype = $(parent).data("filetype");
      console.log(cdxid, tag, dbversion, code, section, filetype);
      $.ajax({
        type: "GET",
        url: "/preview",
        data: {
          tag: tag,
          dbversion: dbversion,
          code: code,
          section: section,
          filetype: filetype,
        },
        success: function (data) {
          // Have to readjust height to avoid expansion
          const previewDiv = $(parent).find("div.preview").first();
          const previewParent = previewDiv.parents("div").first();
          const previewParentHeight = previewParent.height();
          const previewDivHeight = previewDiv.height();
          $(previewDiv).html(data);
          previewParent.height(previewParentHeight);
          previewDiv.height(previewDivHeight);
        },
        error: function () {
          console.error("AJAX error: get_preview");
        },
      });
    },
    function () {}
  );

  // Comment show/hide
  $(".show-comments").click(function () {
    // const codexFile = $(this).closest(".container.codex-file");
    // const parent = $(this).closest("div[class^='codex']");
    const cdxid = $(this).parents(".control-buttons").first().data("cdxid");
    const codexType = cdxid.split("-")[1];
    let code;
    if (codexType === "f") {
      code = $(".codex-file[data-cdxid='" + cdxid + "']")
        .find("code")
        .first();
    } else if (codexType === "c") {
      code = $(".codex-calc[data-cdxid='" + cdxid + "']")
        .find("code")
        .filter(function () {
          return (
            $(this).parents(
              '.container.file-container[data-displaytype="processed"]'
            ).length > 0
          );
        });
    } else if (codexType === "p") {
      code = $(".codex-proj[data-cdxid='" + cdxid + "']")
        .find("code")
        .filter(function () {
          return (
            $(this).parents(
              '.container.file-container[data-displaytype="processed"]'
            ).length > 0
          );
        });
    }

    if ($(this).is(":checked")) {
      code.each(function () {
        $(this).find(".token.comment").show();
        const codexFile = $(code).closest(".container.codex-file");
        const showCommentsButton = $(codexFile).find(".show-comments").first();
        $(showCommentsButton).siblings("label").text("Hide Comments");
        $(showCommentsButton).prop("checked", true);
      });
      $(this).siblings("label").text("Hide Comments");
    } else {
      code.each(function () {
        $(this).find(".token.comment").hide();
        const codexFile = $(code).closest(".container.codex-file");
        const showCommentsButton = $(codexFile).find(".show-comments").first();
        $(showCommentsButton).siblings("label").text("Show Comments");
        $(showCommentsButton).prop("checked", false);
      });
      $(this).siblings("label").text("Show Comments");
    }
  });

  // Copy code on click
  $(".copy-contents").on("click", function () {
    const cdxid = $(this).parents(".control-buttons").first().data("cdxid");
    // Find the code block with the same cdxid and copy it
    code = $(".codex-file[data-cdxid='" + cdxid + "']")
      .find("code")
      .first();
    // This is a pain with jQuery, so just use the DOM
    navigator.clipboard.writeText(code.get(0).innerText);
  });

  // Copy cdx-id on click
  $(".copy-id").on("click", function () {
    const cdxid = $(this).parents(".control-buttons").first().data("cdxid");
    navigator.clipboard.writeText(cdxid);
  });

  // Copy link on click
  $(".copy-link").on("click", function () {
    // Find its first button group parent
    const url = window.location.href;
    navigator.clipboard.writeText(url);
  });

  // Toggle between raw and processed files
  $(".file-select").on("click", function () {
    const parent = $(this).closest(".container.codex-file");
    const displayType = $(this).data("displaytype");
    const [rawCont, procCont] = [
      parent.find('.container.file-container[data-displaytype="raw"]'),
      parent.find('.container.file-container[data-displaytype="processed"]'),
    ];
    // Hide the inactive container and show the active one
    const [activeCont, inactiveCont] =
      displayType === "raw" ? [rawCont, procCont] : [procCont, rawCont];
    activeCont.show();
    inactiveCont.hide();
    // Need to change the active button as well
    const [rawButton, procButton] = [
      parent.find('.file-select[data-displaytype="raw"]'),
      parent.find('.file-select[data-displaytype="processed"]'),
    ];
    const [activeButton, inactiveButton] =
      displayType === "raw" ? [rawButton, procButton] : [procButton, rawButton];
    activeButton.parent().first().addClass("active");
    inactiveButton.parent().first().removeClass("active");
  });
});

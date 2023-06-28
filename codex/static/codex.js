function fixPreviewHeight(uuid) {
  const preview = $("div.preview[data-uuid='" + uuid + "']");
  const inputFile = $("div.input-file[data-uuid='" + uuid + "']");
  const inputFileHeight = inputFile.height();

  const inputFileParent = inputFile.parents("div").first();
  const previewParent = preview.parents("div").first();
  const inputFileParentHeight = inputFileParent.height();

  previewParent.height(inputFileParentHeight);
  previewParent.css({ maxHeight: inputFileParentHeight + "px" });
  preview.height(inputFileHeight);
  preview.css({ maxHeight: inputFileHeight + "px" });
}

window.onload = function () {
  // Hide the raw files on page load
  $('.container.file-container').hide();
  $('.container.file-container[data-type="processed"]').show();
  // Fix the height of the preview pane to match the input file (on load)
  $(".preview").each(function () {
    const uuid = $(this).data("uuid");
    fixPreviewHeight(uuid);
  });
};

$(document).ready(function () {
  // Do the same on each accordion button click
  $(".accordion-button").click(function () {
    const uuid = $(this).data("uuid");
    fixPreviewHeight(uuid);
  });

  // Preview on hover
  $(".tag-link").hover(
    function () {
      const tag = $(this).data("name");
      const section = $(this).data("section");
      const parent = $(this).parents("div.codex-file").first();
      const uuid = $(parent).data("uuid");
      const dbversion = $(parent).data("dbversion");
      const code = $(parent).data("code");
      const filetype = $(parent).data("filetype");
      console.log(uuid, tag, dbversion, code, section, filetype);
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
          const previewDiv = $("div.preview[data-uuid='" + uuid + "']");
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
  $("#btn-show-comments").click(function () {
    if ($(this).is(":checked")) {
      $(".token.comment").show();
      $('label[for="btn-show-comments"]').text("Comments Shown");
    } else {
      $(".token.comment").hide();
      $('label[for="btn-show-comments"]').text("Comments Hidden");
    }
  });

  // Add copy code button
  // TODO: this doesn't look very good and is hard to click if comments are under it
  $("pre").each(function () {
    $(this).prepend(
      '<button class="btn btn-sm btn-outline-primary code-copy" style="float:right; cursor:pointer;">Copy</button>'
    );
  });

  // Copy code on click
  $(".code-copy").on("click", function () {
    const $code = $(this).siblings("code");
    // This is a pain with jQuery, so just use the DOM
    navigator.clipboard.writeText($code.get(0).innerText);

    const $button = $(this);
    $button.removeClass("btn-outline-primary").addClass("btn-primary");
    $button.text("Copied!");
    setTimeout(function () {
      $button.text("Copy");
      $button.removeClass("btn-primary").addClass("btn-outline-primary");
    }, 1000);
  });

  $('.file-select').on('click', function () {
    const type = $(this).data('type');
    const uuid = $(this).data('uuid');
    // Hide all file containers and show only the one we want
    $('.container.file-container[data-uuid="'+uuid+'"]').hide();
    container = $('.container.file-container[data-type="'+type+'"][data-uuid="'+uuid+'"]')
    console.log(container)
    container.show();
    // Deactivate all file select buttons and activate the one we clicked
    $('.file-select[data-uuid="'+uuid+'"]').parent().removeClass('active');
    $(this).parent().addClass('active');
  });
});

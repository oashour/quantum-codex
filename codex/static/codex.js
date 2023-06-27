$(document).ready(function () {
  // Fix the height of the preview pane to match the input file
  $(".preview").each(function() {
    const uuid = $(this).data("uuid");

    const inputFile = $("div.input-file[data-uuid='" + uuid + "']");
    const inputFileHeight = inputFile.height();

    const inputFileParent = inputFile.parents('div').first();
    const previewParent = $(this).parents('div').first();
    const inputFileParentHeight = inputFileParent.height();

    previewParent.height(inputFileParentHeight);
    previewParent.css({"maxHeight":inputFileParentHeight+"px"});
    $(this).height(inputFileHeight);
    $(this).css({"maxHeight":inputFileHeight+"px"});
  });

  // Preview on hover
  $(".tag-link").hover(
    function () {
      const tag = $(this).data("name")
      const section = $(this).data("section")
      const parent = $(this).parents('div.section').first();
      const uuid = $(parent).data("uuid")
      const dbversion = $(parent).data("dbversion")
      const code = $(parent).data("code")
      const filetype = $(parent).data("filetype")
      $.ajax({
        type: 'GET',
        url: "/preview",
        data: { tag: tag, dbversion: dbversion, code: code, section: section, filetype: filetype },
        success: function (data) {
          // Have to readjust height to avoid expansion
          const previewDiv = $("div.preview[data-uuid='" + uuid + "']");
          const previewParent = previewDiv.parents('div').first();
          const previewParentHeight = previewParent.height();
          const previewDivHeight = previewDiv.height();
          $(previewDiv).html(data);
          previewParent.height(previewParentHeight);
          previewDiv.height(previewDivHeight);
        },
        error: function () {
          console.error("AJAX error: get_preview")
        },
      });
    },
    function () {}
  );

  // Copy code button
  $('pre').each(function() {
    const code = $(this).find('code').text();
    $(this).prepend('<button class="btn btn-sm btn-outline-primary code-copy" style="float:right; cursor:pointer;">Copy</button>');
    $(this).find('.code-copy').on('click', function() {
      const $button = $(this);
      navigator.clipboard.writeText(code);

      $button.removeClass('btn-outline-primary').addClass('btn-primary');
      $button.text('Copied!');
      setTimeout(function() {
        $button.text('Copy');
        $button.removeClass('btn-primary').addClass('btn-outline-primary');
      }, 1000);
    });
  });
});

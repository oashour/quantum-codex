$(document).ready(function () {
  // do jQuery
  $(".preview").each(function() {
    const fileid = $(this).data("fileid");

    const inputFile = $("div.input-file[data-fileid='" + fileid + "']");
    const inputFileHeight = inputFile.height();

    const inputFileParent = inputFile.parents('div').first();
    const previewParent = $(this).parents('div').first();
    const inputFileParentHeight = inputFileParent.height();

    previewParent.height(inputFileParentHeight);
    previewParent.css({"maxHeight":inputFileParentHeight+"px"});
    $(this).height(inputFileHeight);
    $(this).css({"maxHeight":inputFileHeight+"px"});
  });

  $(".tag-link").hover(
    function () {
      const tag = $(this).data("name")
      const section = $(this).data("section")
      const parent = $(this).parents('div.section').first();
      const fileid = $(parent).data("fileid")
      const dbversion = $(parent).data("dbversion")
      const code = $(parent).data("code")
      const filetype = $(parent).data("filetype")
      /*
      console.log("tag: " + tag)
      console.log("fileid: " + fileid)
      console.log("dbversion: " + dbversion)
      console.log("code: " + code)
      */
      $.ajax({
        type: 'GET',
        url: "/preview",
        data: { tag: tag, dbversion: dbversion, code: code, section: section, filetype: filetype },
        //dataType: 'json',
        //contentType: 'application/json; charset=utf-8',
        success: function (data) {
          const previewDiv = $("div.preview[data-fileid='" + fileid + "']");
          const previewParent = previewDiv.parents('div').first();
          const previewParentHeight = previewParent.height();
          const previewDivHeight = previewDiv.height();
          //console.log("Original heights are" + previewParentHeight + " and " + previewDivHeight);
          $(previewDiv).html(data);
          // console.log("New height is: " + $(previewDiv).height());
          previewParent.height(previewParentHeight);
          previewDiv.height(previewDivHeight);
          //console.log("Adjusted height is: " + previewParent.height());
        },
        error: function () {
          console.error("AJAX error: get_preview")
        },
      });
    },
    function () {
      // const id = $(this).attr("id")
      //const prev_id = "preview_" + id
      // $("#"+prev_id).hide();
    }
  );
  /*
  $(".left").on("click", function (event) {
    const target = $(event.target);
    // Check if the clicked element is not a preview or a tag-link
    if (!target.hasClass("preview") && !target.hasClass("tag-link")) {
      $(".preview").hide(); // Hide all previews
    });
  */
});

/*
document.querySelectorAll("code").forEach(function(element) {
  hljs.addPlugin(mergeHTMLPlugin);
  hljs.highlightElement(element);
});
*/

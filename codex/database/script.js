$(".tag-link").hover(
  function () {
    const id = $(this).attr("id")
    const file_id = $(this).data("fileid")
    const preview_id = "preview_" + id
    $('.preview').filter(
      // We want to hide all previews from the same file, but not the one we are hovering over
      function () { 
      diff_preview = $(this).attr("id") !== preview_id
      same_file = $(this).data("fileid") === file_id
      return diff_preview && same_file
    }).hide()
      // We want to show the preview for the link we're hovering over 
      // but only in the same file
    $('.preview').filter(
      function () { 
      correct_preview = $(this).attr("id") === preview_id
      correct_file = $(this).data("fileid") === file_id
      return correct_preview && correct_file
    }).show()
  },
  function () {
    // const id = $(this).attr("id")
    //const prev_id = "preview_" + id
    // $("#"+prev_id).hide();
  }
);

$(".left").on("click", function (event) {
  const target = $(event.target);
  // Check if the clicked element is not a preview or a tag-link
  if (!target.hasClass("preview") && !target.hasClass("tag-link")) {
    $(".preview").hide(); // Hide all previews
  }
});

$(".tag-link").hover(
    function(){
      const id = $(this).attr("id")
      const prev_id = "preview_" + id
      $(".preview").not("#" + prev_id).hide();
      $("#"+prev_id).show();
      }, 
    function(){
      // const id = $(this).attr("id")
      //const prev_id = "preview_" + id
      // $("#"+prev_id).hide();
    }
);

$(".left").on("click", function(event) {
    const target = $(event.target);
    // Check if the clicked element is not a preview or a tag-link
    if (!target.hasClass("preview") && !target.hasClass("tag-link")) {
        $(".preview").hide(); // Hide all previews
    }
});

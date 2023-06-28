// Adjusts the options for the database version select element based on the code
// TODO: this should be pulled from the database or something? Shouldn't be hard coded...
$(document).ready(function() {
    const codeSelect = $('select[name="code"]');
    const dbVersionSelect = $('select[name="dbversion"]');

    function updateDbVersionOptions() {
        const selectedCode = codeSelect.val();
        if (selectedCode === 'Quantum ESPRESSO') {
        dbVersionSelect.html(`
            <option selected>7.2 (latest)</option>
            <option>7.1</option>
            <option>7.0</option>
            <option>6.8</option>
            <option>6.7</option>
        `);
        } else if (selectedCode === 'VASP') {
        dbVersionSelect.html(`
            <option selected>Latest</option>
        `);
        }
    }

    // Update options when page is first created
    updateDbVersionOptions();

    // Update options when code select element is changed
    codeSelect.on('change', updateDbVersionOptions);

  const fileInput = $('#dft-file input[type=file]');
  fileInput.on('change', function() {
    if (fileInput[0].files.length > 0) {
      const fileList = $('#file-list');
      fileList.empty();
      for (let i = 0; i < fileInput[0].files.length; i++) {
        fileList.append(`<li>${fileInput[0].files[i].name}</li>`);
      }
    }
  });
});
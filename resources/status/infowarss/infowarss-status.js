$(document).ready(function() {
    $('.datatable').DataTable({
	"paging":   false,
	"searching":   false,
    });
    var sources_datatable = $('#sources-datatable').DataTable({
	"paging":   false,
	"searching":   false,
    });

    $('#sources-datatable').on('click', 'td.details-control', function () {
        var tr = $(this).closest('tr');
        var row = sources_datatable.row(tr);

        if (row.child.isShown()) {
            row.child.hide();
            tr.removeClass('shown');
        } else {
            row.child(tr.data('child-value')).show();
            tr.addClass('shown');
        }
    });
});

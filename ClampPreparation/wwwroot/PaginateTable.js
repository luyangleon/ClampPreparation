(function ($) {
    $.fn.paginateTable = function (options) {
        // 默认设置
        var settings = $.extend({
            rowsPerPage: 10,
            currentPage: 1,
            prevText: 'Previous',
            nextText: 'Next',
        }, options);

        // 实现分页逻辑
        var paginate = function (table, rowsPerPage, currentPage) {
            var rows = table.find('tbody tr');
            var totalRows = rows.length;
            var totalPages = Math.ceil(totalRows / rowsPerPage);

            // 隐藏所有行
            rows.hide();

            // 显示当前页的行
            var start = (currentPage - 1) * rowsPerPage;
            var end = start + rowsPerPage;
            rows.slice(start, end).show();

            // 更新分页导航
            var paginationNav = table.next('.pagination-nav');
            if (paginationNav.length === 0) {
                paginationNav = $('<div class="pagination-nav"></div>');
                table.after(paginationNav);
            }
            paginationNav.empty();

            // 添加分页按钮
            if (currentPage > 1) {
                var prevButton = $('<button class="prev-page">' + settings.prevText + '</button>');
                prevButton.on('click', function () {
                    settings.currentPage--;
                    paginate(table, settings.rowsPerPage, settings.currentPage);
                });
                paginationNav.append(prevButton);
            }

            if (currentPage < totalPages) {
                var nextButton = $('<button class="next-page">' + settings.nextText + '</button>');
                nextButton.on('click', function () {
                    settings.currentPage++;
                    paginate(table, settings.rowsPerPage, settings.currentPage);
                });
                paginationNav.append(nextButton);
            }
        };

        // 初始化插件
        return this.each(function () {
            var table = $(this);
            paginate(table, settings.rowsPerPage, settings.currentPage);
        });
    };
}(jQuery));
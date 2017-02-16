var path = require('path');

var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var HtmlWebpackInlineSourcePlugin = require('html-webpack-inline-source-plugin');
var CleanCSSPlugin = require('less-plugin-clean-css');

var dir_src = path.resolve(__dirname, 'src');
var dir_html = path.resolve(__dirname, 'html');
var dir_build = path.resolve(__dirname, 'build');

module.exports = {
    entry: {bundle: [path.resolve(dir_src, 'index.js')]},
    output: {
        path: dir_build,
        filename: 'bundle.js',
        library: "QuastEvaluator"
    },
    devServer: {
        contentBase: dir_build
    },
    module: {
        loaders: [
            {
                test: /\.(ttf|eot|svg|woff(2)?)(\?[a-z0-9]+)?$/,
                loader: 'file-loader'
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: "url-loader?limit=10000&mimetype=application/font-woff"
            },
            {test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/, loader: "file-loader"},
            {test: /\.css$/, loader: "style-loader!css-loader"},
            {
                test: /\.less$/,
                use: [
                    'style-loader',
                    { loader: 'css-loader', options: {importLoaders: 1}},
                    {
                        loader: 'less-loader',
                        options: {lessPlugins: [new CleanCSSPlugin({advanced: true})]}
                    }
                ]
            },
            {test: /\.handlebars$/, loader: "handlebars-loader?helperDirs[]=" + dir_src + "/helpers"}
        ]
    },
    plugins: [
        // Simply copies the files over
        new HtmlWebpackPlugin({
            title: '',
            filename: 'index.html',
            inlineSource: '.(js|css)$',
            template: dir_html + '/index.ejs',
            minify: {minifyJS: true, minifyCSS: true, removeComments: true}
        }),
        new HtmlWebpackInlineSourcePlugin(),
        new webpack.ProvidePlugin({
            $: "jquery",
            jQuery: "jquery"
        }),
        // Avoid publishing files when compilation fails
        new webpack.NoEmitOnErrorsPlugin()
    ],
    stats: {
        // Nice colored output
        colors: true
    }
};

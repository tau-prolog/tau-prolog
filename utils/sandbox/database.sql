-- phpMyAdmin SQL Dump
-- version 4.0.9
-- http://www.phpmyadmin.net
--
-- Servidor: 10.6.64.97
-- Tiempo de generación: 23-10-2018 a las 13:30:32
-- Versión del servidor: 5.5.59
-- Versión de PHP: 5.3.3

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Base de datos: `db5460196_tpcodes`
--

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `code`
--

CREATE TABLE IF NOT EXISTS `code` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `link` char(8) CHARACTER SET utf8 COLLATE utf8_bin NOT NULL,
  `olink` char(8) NOT NULL,
  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `code` text CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `name` tinytext NOT NULL,
  `description` text NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `link` (`link`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=146 ;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
